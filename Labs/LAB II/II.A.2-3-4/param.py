# This file contains the parameters of the simulation

n_buses = 10 # number of buses
n_buses_list = [5, 7, 10, 15] # list of number of buses for utilization comparison
n_simulations = 15 # number of simulations
c = 20 # number of seats in each bus
q = 0.3 # probability of a passenger leaving at a stop

travel_times = {
    'R 1': 3, 'R 2': 7, 'R 3': 6, 
    'R 4': 1, 'R 5': 4, 'R 6': 3, 
    'R 7': 9, 'R 8': 1, 'R 9': 3, 
    'R 10': 8, 'R 11': 8, 'R 12': 5, 
    'R 13': 6, 'R 14': 2, 'R 15': 3
}

passenger_arrival_rates = {
    'S 1 e': 0.3, 'S 1 w': 0.6, 
    'S 2 e': 0.1, 'S 2 w': 0.1,
    'S 3 e': 0.3, 'S 3 w': 0.9,
    'S 4 e': 0.2, 'S 4 w': 0.5,
    'S 5 e': 0.6, 'S 5 w': 0.4,
    'S 6 e': 0.6, 'S 6 w': 0.4,
    'S 7 e': 0.6, 'S 7 w': 0.4
}

routes = {
    "1 eastbound": {
        "S": ["E 1", "S 1 e", "S 4 e", "S 6 e", "E 3"],
        "R": ["R 1", "R 5", "R 8", "R 13"]
    },
    "1 westbound": {
        "S": ["E 3", "S 6 w", "S 4 w", "S 1 w", "E 1"],
        "R": ["R 13", "R 8", "R 5", "R 1"]
    },
    "2 eastbound": {
        "S": ["E 1", "S 1 e", "S 4 e", "S 7 e", "E 4"],
        "R": ["R 1", "R 5", "R 10", "R 15"]
    },
    "2 westbound": {
        "S": ["E 4", "S 7 w", "S 4 w", "S 1 w", "E 1",],
        "R": ["R 15", "R 10", "R 5", "R 1"]
    },
    "3 eastbound": {
        "S": ["E 2", "S 2 e", "S 5 e", "S 6 e", "E 3"],
        "R": ["R 3", "R 7", "R 9", "R 13"]
    },
    "3 westbound": {
        "S": ["E 3", "S 6 w", "S 5 w", "S 2 w", "E 2"],
        "R": ["R 13", "R 9", "R 7", "R 3"]
    },
    "4 eastbound": {
        "S": ["E 2", "S 3 e", "S 7 e", "E 4"],
        "R": ["R 4", "R 12", "R 15"]
    }, 
    "4 westbound": {
        "S": ["E 4", "S 7 w", "S 3 w", "E 2"],
        "R": ["R 15", "R 12", "R 4"]
    }
}