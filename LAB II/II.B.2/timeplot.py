import numpy as np
import matplotlib.pyplot as plt

# Data for average utilization and standard error
num_buses = [5, 7, 10, 15]

average_travel_time = [20, 18.9, 18.9, 17.5]
time_standard_error = [0.757, 0.555, 0.445, 0.194]

np.random.seed(0) 

# Create a plot with error bars
plt.figure()
plt.errorbar(num_buses, average_travel_time, yerr=time_standard_error, fmt='o', capsize=5, label="Average Travel Time", color='r')
plt.title('Average Travel Time with Standard Error for Different Bus Counts', size=21, fontdict={'family': 'serif'})
plt.xlabel('Number of Buses', fontdict={'family': 'serif'}, size=17)
plt.ylabel('Average Travel Time', fontdict={'family': 'serif'}, size=17)
plt.xticks(ticks=num_buses, labels=[str(bus) for bus in num_buses], fontsize=14) 
plt.yticks(fontsize=14)

plt.grid(True)
plt.show()

