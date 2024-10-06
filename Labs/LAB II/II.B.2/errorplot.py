import numpy as np
import matplotlib.pyplot as plt

# Data for average utilization and standard error
num_buses = [5, 7, 10, 15]
average_utilization = [0.426, 0.363, 0.305, 0.216]
util_standard_error = [0.015, 0.017, 0.009, 0.004]

average_utilization_random = [0.301, 0.234, 0.172, 0.125]
util_standard_error_random = [0.007, 0.006, 0.003, 0.001]


np.random.seed(0) 

# Create a plot with error bars
plt.figure(figsize=(8, 6))
plt.errorbar(num_buses, average_utilization_random, yerr=util_standard_error_random, fmt='o', capsize=5, label="Random Route Selection", color='r')
plt.errorbar(num_buses, average_utilization, yerr=util_standard_error, fmt='o', capsize=5, label="Implemented Route Selection", color='g')

# Labeling the plot
plt.title('Average Utilisation with Standard Error for Different Bus Counts', size=21, fontdict={'family': 'serif'})
plt.xlabel('Number of Buses', fontdict={'family': 'serif'}, size=17)
plt.ylabel('Average Utilization', fontdict={'family': 'serif'}, size=17)
plt.xticks(ticks=num_buses, labels=[str(bus) for bus in num_buses], fontsize=14) 
plt.yticks(fontsize=14)
plt.legend(fontsize=14)
plt.grid(True)
plt.show()

