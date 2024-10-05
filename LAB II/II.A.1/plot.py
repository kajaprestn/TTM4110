import numpy as np
import matplotlib.pyplot as plt

# Parameters
lambda_ = 0.5  # Rate parameter (lambda = 0.5 for this example)

# Define the PDF function for the exponential distribution
def exponential_pdf(t, lambda_):
    return lambda_ * np.exp(-lambda_ * t)

# Generate values for t
t_values = np.linspace(0, 15, 1000)

# Compute the PDF values for each t
pdf_values = exponential_pdf(t_values, lambda_)

# Plot the PDF
plt.plot(t_values, pdf_values, label=r'$f(t) = \lambda e^{-\lambda t}$', color='red')
plt.title('Exponential Probability Density Function', size=21, fontdict={'family': 'serif'})
plt.xlabel('t (Time between arrivals)', size=17, fontdict={'family': 'serif'})
plt.ylabel('f(t)', size=17, fontdict={'family': 'serif'})
plt.xticks(fontsize=14)
plt.yticks(fontsize=14)
plt.grid(True)
plt.legend(fontsize=14)
plt.show()
