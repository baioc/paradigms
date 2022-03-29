"""
Optimal binary partition aka 0/1 Knapsack aka Subset sum problem.

For each value in an input array, assign it to either partition A or B.
This partitioning should be done so as to optimize some integer equation, e.g.
    y = w0*x0 + w1*x1 + ... + wN*xN + delta
where xi is an unknown integer representing the assignment given to wi.

Example: balance the load of two servers, inputs are estimated task loads.
Balancing implies *minimizing the absolute difference* between server loads.
Consider that each xi can be either +1 (server A) or -1 (server B); if delta=0,
y is the load difference. Then, the goal can be either to to find x's which
minimize abs(y), or simply the minimum possible value for abs(y).
"""
class Partition:

    def bruteforce(inputs):
        n = len(inputs)

        def dfs(node, total):
            # base case
            if node >= n: return total

            # recursive case evaluates all options
            left = dfs(node + 1, total + inputs[node])
            right = dfs(node + 1, total - inputs[node])

            # make a choice at this node
            choice = right if abs(right) < abs(left) else left
            return choice

        solution = abs(dfs(0, 0))
        return solution


    def memoized(inputs):
        n = len(inputs)
        cache = {}

        def dfs(node, total):
            # cache hit
            key = (node, abs(total))
            if key in cache:
                return cache[key]

            # base case
            if node >= n: return total

            # recursive case evaluates all options
            left = dfs(node + 1, total + inputs[node])
            right = dfs(node + 1, total - inputs[node])

            # make a choice at this node
            choice = right if abs(right) < abs(left) else left
            cache[key] = choice
            return choice

        solution = abs(dfs(0, 0))
        return solution


if __name__ == '__main__':
    cases = {
        (1, 2, 3, 4, 5): 1,
        (19, 1, 8, 2):   8,
    }
    for example, solution in cases.items():
        assert Partition.bruteforce(example) == solution
        assert Partition.memoized(example) == solution
