--daily statistics
SELECT timestamp::date AS day, 
	final_response_preferred != -1 AS responded, 
	count(*) 
FROM quizzes_quizresponse 
WHERE quiz_id=41 AND timestamp::date >= now()::date - INTERVAL '6 days' 
GROUP BY day, responded
ORDER BY day, responded;

--hourly statistics
SELECT timestamp::date AS date, 
	extract(hour from timestamp) AS hour, 
	final_response_preferred != -1 AS responded, 
	count(*) 
FROM quizzes_quizresponse 
WHERE quiz_id=41 AND timestamp >= now() - INTERVAL '1 day' 
GROUP BY hour, responded, date 
ORDER BY date, hour, responded;

--try making entire data structure in SQL

DROP TABLE IF EXISTS daily_counts;
CREATE TEMPORARY TABLE daily_counts AS (
	SELECT day, responded, 
	CASE when count is not null then count else 0 END as count FROM (
	SELECT * FROM (
	SELECT day::date
	FROM generate_series(now()::timestamp,
						 now()::timestamp - interval '7 days',
						 interval '-1 day') day
	) d 
	CROSS JOIN (
		SELECT responded FROM unnest(ARRAY[true,false]) as responded
		) c ) cd
FULL OUTER JOIN 
		SELECT timestamp::date AS day, 
			final_response_preferred != -1 AS responded, 
			count(*) 
		FROM quizzes_quizresponse 
		WHERE quiz_id=41 AND timestamp::date >= now()::date - INTERVAL '7 days' 
		GROUP BY day, responded
		ORDER BY day, responded
	) t USING (day, responded));

SELECT * FROM (
	SELECT day, count as feedback FROM daily_counts where responded=true) t1
	JOIN
	(SELECT day, count as nofeedback FROM daily_counts where responded=false) t2
	USING (day);

DROP TABLE IF EXISTS daily_counts;

--now do hourly

DROP TABLE IF EXISTS hourly_counts;


CREATE TEMPORARY TABLE hourly_counts AS (
	SELECT dayhour, responded, 
	CASE when count is not null then count else 0 END as count FROM (
	SELECT * FROM (
	SELECT dayhour
	FROM generate_series(date_trunc('hour',now())::timestamp,
						 date_trunc('hour',now())::timestamp - interval '1 day',
						 interval '-1 hour') dayhour
	) d 
	CROSS JOIN (
		SELECT responded FROM unnest(ARRAY[true,false]) as responded
		) c ) cd
FULL OUTER JOIN (
		SELECT date_trunc('hour', timestamp) AS dayhour, 
			final_response_preferred != -1 AS responded, 
			count(*) 
		FROM quizzes_quizresponse 
		WHERE quiz_id=41 AND date_trunc('hour', timestamp) >= date_trunc('hour', now())
		 - INTERVAL '1 day' 
		GROUP BY dayhour, responded
		ORDER BY dayhour, responded
	) t USING (dayhour, responded));

SELECT * FROM (
	SELECT dayhour, count as feedback FROM hourly_counts where responded=true) t1
	JOIN
	(SELECT dayhour, count as nofeedback FROM hourly_counts where responded=false) t2
	USING (dayhour);

DROP TABLE IF EXISTS hourly_counts;

--NUMERIC - MIN, MAX, MEAN, MEDIAN, 25th & 75th percentile, MODE, SD, SKEW, N_UNIQUE
--CATEGORICAL, radio-choice - # in each category, # of empty categories
--CATEGORICAL, checkbox - for each subquestion, proportion checked

--numeric
--extract all numeric at once
DROP TABLE IF EXISTS numeric_responses;
CREATE TEMPORARY TABLE numeric_responses AS (
	SELECT (response_data::json->number->0)::text::real AS response, question_id, number
	FROM 
	(
	SELECT id AS question_id, number FROM quizzes_quizquestion 
	WHERE quiz_id=41 AND qtype='numeric') t1
	CROSS JOIN (
	SELECT id AS response_id, response_data FROM quizzes_quizresponse
	WHERE quiz_id=41 LIMIT 10000) t2 
	);

SELECT * FROM 
	((SELECT number, question_id, min(response), max(response), avg(response) as mean, stddev(response),
	percentile_cont(array_agg(response::real), 0.25) as Q25, percentile_cont(array_agg(response::real), 0.5) as median,
	percentile_cont(array_agg(response::real), 0.75) as Q75, count(distinct response) as n_unique 
     FROM numeric_responses GROUP BY question_id, number) t1 
	LEFT JOIN LATERAL (SELECT id, prompt FROM quizzes_quizquestion WHERE quiz_id=41) t2
	ON (t1.question_id = t2.id));

DROP TABLE IF EXISTS numeric_responses;

--categorical, radio-choice

--first, aggregate into the format
--question_id, number, option_number, option_text, count

--question_id, number, ARRAY response vector
DROP TABLE IF EXISTS radio_responses;
CREATE TEMPORARY TABLE radio_responses AS(
	SELECT question_id, number, unnest(ARRAY[response_data::json->number]) as arr
	FROM (
	SELECT id as question_id, number from quizzes_quizquestion
	WHERE quiz_id=41 AND qtype='radio') t1
	CROSS JOIN (
		SELECT id AS response_id, response_data FROM quizzes_quizresponse
		WHERE quiz_id=41 LIMIT 10000) t2);

SELECT t2.question_id, number, t2.option_number, count, text FROM (
	SELECT question_id, number, option_number, count(*) FROM 
		(SELECT question_id, number, 
		position('1' in translate(arr::text,'[],',''))-1 as option_number 
		FROM radio_responses) t1
		GROUP BY question_id, number, option_number) t2
	LEFT JOIN (SELECT question_id, text, number as option_number 
		FROM quizzes_quizquestionoption) t3
	ON (t2.question_id=t3.question_id AND t2.option_number=t3.option_number)
	ORDER BY question_id, number, option_number;

DROP TABLE IF EXISTS radio_responses;

--categorical, checkbox-choice
DROP TABLE IF EXISTS checkbox_responses;
CREATE TEMPORARY TABLE checkbox_responses AS(
	SELECT question_id, number, unnest(ARRAY[response_data::json->number]) as arr
	FROM (
	SELECT id as question_id, number from quizzes_quizquestion
	WHERE quiz_id=41 AND qtype='checkbox') t1
	CROSS JOIN (
		SELECT id AS response_id, response_data FROM quizzes_quizresponse
		WHERE quiz_id=41 LIMIT 10000) t2);


DROP TABLE IF EXISTS checkbox_summary;

CREATE TEMPORARY TABLE checkbox_summary AS 
SELECT v1.question_id, v1.option_number, v1.response, count
FROM (SELECT question_id, generate_series(min, max, 1) AS option_number, response 
	FROM 
(select max(number), min(number), question_id from quizzes_quizquestionoption 
where question_id in (select id from quizzes_quizquestion 
	where quiz_id=41 and qtype='checkbox') GROUP BY question_id) u1
	CROSS JOIN 
	(SELECT response FROM unnest(ARRAY[0,1]) as response
		) u2
) v1
LEFT JOIN (select question_id, response, option_number, count(*) FROM (
	select checkbox_responses.question_id as question_id, u.arr::text::int 
	AS response, u.ord -1 as option_number 
		from checkbox_responses, json_array_elements(arr) 
		with ordinality as u(arr, ord) ) t1 
	GROUP BY question_id, response, option_number) v2
	ON (v1.question_id=v2.question_id AND
	 v1.option_number=v2.option_number AND v1.response=v2.response);

SELECT t1.question_id, t1.option_number, t2.text, t1.response, t1.count FROM 
(SELECT question_id, option_number, response, count FROM 
checkbox_summary) t1
LEFT JOIN 
(SELECT question_id, number as option_number, text FROM quizzes_quizquestionoption) t2
ON (t1.question_id=t2.question_id AND t1.option_number=t2.option_number);

--simply return all question prompts
SELECT id as question_id, number as question_number, prompt, qtype FROM 
quizzes_quizquestion WHERE quiz_id=41;



DROP TABLE IF EXISTS checkbox_summary;
DROP TABLE IF EXISTS checkbox_responses;


--summarize responses
SELECT number, text, 
CASE WHEN count is not null THEN count ELSE 0 END as count
FROM
(SELECT number, text FROM quizzes_quizoption WHERE quiz_id=41) t1
LEFT JOIN
(SELECT final_response_preferred, count(*) FROM quizzes_quizresponse 
WHERE final_response_preferred != -1 AND quiz_id=41 
GROUP BY final_response_preferred) t2
ON (t1.number=t2.final_response_preferred) ORDER BY number;


--previous definition
--found somewhere online
--finds quantile of real column and does linear interpolation between closest values
CREATE OR REPLACE FUNCTION public.percentile_cont(myarray real[], percentile real)
 RETURNS real
 LANGUAGE plpgsql
 IMMUTABLE
AS $function$

DECLARE
  ary_cnt INTEGER;
  row_num real;
  crn real;
  frn real;
  calc_result real;
  new_array real[];
BEGIN
  ary_cnt = array_length(myarray,1);
  row_num = 1 + ( percentile * ( ary_cnt - 1 ));
  new_array = array_sort(myarray);

  crn = ceiling(row_num);
  frn = floor(row_num);

  if crn = frn and frn = row_num then
    calc_result = new_array[row_num];
  else
    calc_result = (crn - row_num) * new_array[frn] 
            + (row_num - frn) * new_array[crn];
  end if;

  RETURN calc_result;
END;
$function$

