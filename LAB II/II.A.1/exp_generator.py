import matplotlib.pyplot as plt
import numpy as np

mean_interarrival_time = 2
lambda_ = 1 / mean_interarrival_time 
n_samples = 1000

def generate_exp_samples(mean_interarrival_time, n_samples):
    u = np.random.uniform(0, 1, n_samples)
    return - mean_interarrival_time * np.log(u)

X_samples = generate_exp_samples(mean_interarrival_time, n_samples)
Y_samples = np.random.exponential(mean_interarrival_time, n_samples)

X_sorted = np.sort(X_samples)
Y_sorted = np.sort(Y_samples)

plt.plot(X_sorted, Y_sorted, marker='o', linestyle='none', color='red')
plt.xlabel('X(j)', size=17, fontdict={'family': 'serif'})
plt.ylabel('Y(j)', size=17, fontdict={'family': 'serif'})
plt.title('Plot of X(j) and Y(j)', size=21, fontdict={'family': 'serif'})
plt.xticks(fontsize=14)
plt.yticks(fontsize=14)
plt.legend()
plt.grid()
plt.show()


