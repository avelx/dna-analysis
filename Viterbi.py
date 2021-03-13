# Code adapted from Wikipedia

####################################################
############  START MODIFICATIONS HERE  ############
####################################################

states = ('E', 'D1', 'D2', 'I', 'A1', 'A2')

# THHHH -> TTHHH
observations = ('heads', 'tails', 'heads')

start_probability = {'Fair': 0.857, 'Biased': 0.142}

transition_probability = {
    'Fair' : {'Fair': 0.533, 'Biased': 0.466},
    'Biased' : {'Fair': 0.60, 'Biased': 0.40}
}

emission_probability = {
    'Fair' : {'heads': 0.857, 'tails': 0.533},
    'Biased' : {'heads': 0.142, 'tails': 0.466}
}

###################################################
############  END MODIFICATIONS HERE  #############
###################################################

def viterbi(obs, states, start_p, trans_p, emit_p):
    V = [{}]
    path = {}

    # Initialize base cases (t == 0)
    for y in states:
        V[0][y] = start_p[y] * emit_p[y][obs[0]]
        path[y] = [y]

    # Run Viterbi for t > 0
    for t in range(1, len(obs)):
        V.append({})
        newpath = {}

        for y in states:
            (prob, state) = max((V[t-1][y0] * trans_p[y0][y] * emit_p[y][obs[t]], y0) for y0 in states)
            V[t][y] = prob
            newpath[y] = path[state] + [y]

        # Don't need to remember the old paths
        path = newpath
    n = 0           # if only one element is observed max is sought in the initialization values
    if len(obs) != 1:
        n = t
    print_dptable(V)
    (prob, state) = max((V[n][y], y) for y in states)
    return (prob, path[state])

# Don't study this, it just prints a table of the steps.
def print_dptable(V):
    s = "    " + " ".join(("%7d" % i) for i in range(len(V))) + "\n"
    for y in V[0]:
        s += "%.5s: " % y
        s += " ".join("%.7s" % ("%f" % v[y]) for v in V)
        s += "\n"
    print(s)

def example():
    return viterbi(observations,
                   states,
                   start_probability,
                   transition_probability,
                   emission_probability)
print(example())
