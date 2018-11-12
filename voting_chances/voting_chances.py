import scipy.stats
import math
import numpy as np
from itertools import izip_longest

def calculate_majority(x):
    if x % 2 == 0:
        return x//2 + 1
    else:
        return (x-1)//2 + 1

def calculate_prior_probability_probabilities(
        prob1,
        error1,
        prob2=None,
        error2=None,
):
    n_sections = 30


    percentiles = np.arange(0.05,0.95001,1.000/n_sections)
    # just calculate deviation
    prob_vals = [
        scipy.stats.norm.ppf(p, 0, error1/1.96)
        for p in percentiles
    ]

    #margin = 1.96 * math.sqrt(n_votes*prob*(1-prob))
    
    prob_dev_centers = [
        (prob_vals[i]+prob_vals[i-1])/2
        for i in range(1, len(prob_vals))
    ]

    # we are excluding the most extreme probabilities
    prob_unweighted_probs = [
        (
            percentiles[i]-percentiles[i-1]
        )
        for i in range(1, len(prob_vals))
    ]

    prob1_centers = [
        prob1+center for center in prob_dev_centers
    ]
    if prob2 is not None:
        if error2 is None:
            error2 = error1
        prob2_centers = [
            prob2 - error2/error1 * center for center in prob_dev_centers
        ]
    else:
        prob2_centers = [None]

    prob_weighted_probs = np.asarray(prob_unweighted_probs)/np.sum(prob_unweighted_probs)

    return prob1_centers, prob2_centers, prob_weighted_probs
    

    
def calculate_vote_probability(
        n_votes,
        n_added,
        prob1,
        error1=None,
        prob2=None,
        error2=None,
        use_plurality=True,
        use_error=True
):
    if error1 is None:
        use_error=False
    if use_error:
        # integrate probabilities over the most common
        # (and anticorrelated if 2) percentages

        # calculate prob lists
        prob1_vals, prob2_vals, prob_probs = calculate_prior_probability_probabilities(
            prob1,
            error1,
            prob2,
            error2
        )

        #for v1, v2, v3 in zip(prob1_vals, prob2_vals, prob_probs):
        #print v1, v2, v3

        # iterate over both changing
        integrated_results = [
            calculate_vote_probability(
                n_votes,
                n_added,
                prob1_val,
                None,
                prob2_val,
                None,
                use_plurality,
                False,
            )
            for prob1_val, prob2_val in izip_longest(
                    prob1_vals, prob2_vals
            )
        ]
        #print prob1_vals, prob2_vals
        #print integrated_results
        #print list(izip_longest(prob1_vals, prob2_vals))

        # summarize
        probability_of_making_a_difference = sum([
            p * result[0]
            for p, result in zip(prob_probs, integrated_results)
        ])

        probability_of_winning = sum([
            p * result[1]
            for p, result in zip(prob_probs, integrated_results)
        ])

        return probability_of_making_a_difference, probability_of_winning

        
    else:
        # just disable these
        if not use_plurality:
            prob2=None
            error2=None

        if prob2 is None:
            # 50/50 split is acceptable for even values here
            # n_added isn't included in 
            n_votes_to_win = calculate_majority(n_votes)
            # needs *at least* this many votes
            probability_of_winning = 1-scipy.stats.binom.cdf(
                n_votes_to_win-n_added-1,
                n_votes,
                prob1
            )
            #print probability_of_winning, n_votes_to_win, n_votes, prob1
            # calculate 
            n_votes_to_win_without_added = calculate_majority(n_votes)
            probability_of_winning_without_added = 1-scipy.stats.binom.cdf(
                n_votes_to_win_without_added-1,
                n_votes,
                prob1
            )
            if n_votes % 2 == 0:
                # tiebreaker prob
                # tie previously count as a win
                probability_of_winning_without_added = probability_of_winning_without_added + 0.5 * scipy.stats.binom.pmf(n_votes//2, n_votes, prob1)

            # tiebreaker for added votes slightly different calculation
            if (n_votes + n_added) % 2 == 0:
                probability_of_winning = probability_of_winning + 0.5 * scipy.stats.binom.pmf(n_votes_to_win-n_added-1, n_votes, prob1)


            probability_of_making_a_difference = probability_of_winning - probability_of_winning_without_added

            return probability_of_making_a_difference, probability_of_winning
        else:
            # scale probability to only count two
            prob_modified = prob1 / (prob1 + prob2)
            prob2_modified = prob2/(prob1+prob2)
            #print prob_modified
            # integrate probabilities over the most common n_total
            # ignore margin of error, as it will mostly cancel out
            # (2nd order approximation)

            n_votes_vals, prob_n_votes_vals = calculate_n_votes_probs(
                n_votes,
                prob1 + prob2,
            )
            #print(prob_n_votes_vals)
            
            integrated_results = [
                calculate_vote_probability(
                    n_votes_i,
                    n_added,
                    prob1=prob_modified,
                )
                for n_votes_i in n_votes_vals
            ]

            probability_of_making_a_difference = sum([
                p * result[0]
                for p, result in zip(prob_n_votes_vals, integrated_results)
            ])

            probability_of_winning = sum([
                p * result[1]
                for p, result in zip(prob_n_votes_vals, integrated_results)
            ])

            return probability_of_making_a_difference, probability_of_winning
        
            
def calculate_n_votes_probs(
        n_votes,
        prob
):
    # determine optimal number and range of probabilities
    peak = round(n_votes * prob)
    # 30 will include the midpoint and both endpoints
    n_sections = min(n_votes, 30)

    percentiles = np.arange(0.05,0.95001,1.000/n_sections)
    counts = [scipy.stats.binom.ppf(p, n_votes, prob) for p in percentiles]

    #margin = 1.96 * math.sqrt(n_votes*prob*(1-prob))
    
    vote_centers = [
        round((counts[i]+counts[i-1])/2)
        for i in range(1, len(counts))
    ]

    # we are excluding the most extreme probabilities
    vote_unweighted_probs = [
        (
            percentiles[i]-percentiles[i-1]
        )
        for i in range(1, len(vote_centers))
    ]

    vote_weighted_probs = np.asarray(vote_unweighted_probs)/np.sum(vote_unweighted_probs)

    return vote_centers, vote_weighted_probs
    

