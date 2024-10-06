import matplotlib.pyplot as plt
import numpy as np

# Data for buses and restrictions
buses = [5, 7, 10, 15]
restrictions = [1, 2, 3, 4]

# Utilization data
utilization_mean = {
    1: [0.433, 0.356, 0.318, 0.23],
    2: [0.424, 0.376, 0.318, 0.239],
    3: [0.452, 0.373, 0.319, 0.223],
    4: [0.43, 0.374, 0.284, 0.234]
}

utilization_std = {
    1: [0.019, 0.017, 0.008, 0.006],
    2: [0.025, 0.02, 0.008, 0.007],
    3: [0.019, 0.012, 0.012, 0.007],
    4: [0.018, 0.015, 0.013, 0.007]
}

# Travel time data
travel_time_mean = {
    1: [20.4, 18.2, 17.5, 18.2],
    2: [20.2, 19.7, 18.9, 19.2],
    3: [19.8, 19.4, 19.4, 19.1],
    4: [19.8, 19.1, 18.3, 19.2]
}

travel_time_std = {
    1: [0.573, 0.597, 0.374, 0.547],
    2: [0.722, 0.692, 0.785, 0.536],
    3: [0.737, 0.787, 0.614, 0.823],
    4: [0.794, 0.582, 0.728, 0.735]
}
# Plot Utilization without STD
plt.figure()
for r in restrictions:
    plt.plot(buses, utilization_mean[r], label=f'max = {r}', marker='o')

plt.title('Average Utilization for Different Bus Counts and Restrictions', fontdict={'family': 'serif'}, size=21)
plt.xlabel('Number of Buses', fontdict={'family': 'serif'}, size=17)
plt.ylabel('Average Utilization', fontdict={'family': 'serif'}, size=17)
plt.xticks(ticks=buses, labels=[str(bus) for bus in buses], fontsize=14) 
plt.yticks(fontsize=14)
plt.legend(title='Bus Restriction per Route', fontsize=14)
plt.grid(True)
plt.tight_layout()
plt.show()

# Plot Travel Time without STD
plt.figure()
for r in restrictions:
    plt.plot(buses, travel_time_mean[r], label=f'max = {r}', marker='o')

plt.title('Average Travel Time for Different Bus Counts and Restrictions', fontdict={'family': 'serif'}, size=21)
plt.xlabel('Number of Buses', fontdict={'family': 'serif'}, size=17)
plt.ylabel('Average Travel Time', fontdict={'family': 'serif'}, size=17)
plt.xticks(ticks=buses, labels=[str(bus) for bus in buses], fontsize=14) 
plt.yticks(fontsize=14)
plt.legend(title='Bus Restriction per Route', fontsize=14)
plt.grid(True)
plt.tight_layout()
plt.show()

