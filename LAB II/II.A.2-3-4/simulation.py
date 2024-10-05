import numpy as np
import simpy as sp
import param
from bus import Bus, choose_init_route

# Function to generate passengers at random bus stops
def passenger_generator(env, passenger_container, passenger_arrival_rate):
    while True:
        yield env.timeout(np.random.exponential(1/passenger_arrival_rate))
        passenger_container.put(1)

# Function for the simulation
def simulation(n_buses, n_simulations):
    utils = []  # List to store average utilization per simulation
    for i in range(n_simulations):
        env = sp.Environment()

        all_stops = set() # Set to store all stops
        for route in param.routes.values():
            stops_in_route = route['S']
            for stop in stops_in_route:
                all_stops.add(stop)
            
        passenger_containers = {} # Dictionary to store passengers waiting at each stop
        for stop in all_stops:
            passenger_containers[stop] = sp.Container(env, capacity=float('inf'), init=0)

        util = {} # Utilization tracking for each bus
        for bus_id in range(n_buses):
            util[bus_id] = []
            
        # Start passenger generators for each stop with their respective rates
        passenger_arrival_rates = param.passenger_arrival_rates
        for stop, rate in passenger_arrival_rates.items():
            env.process(passenger_generator(env, passenger_containers[stop], rate))
        
        # Start bus processes
        for id in range(n_buses):
            bus = Bus(env, param.routes, choose_init_route(), passenger_containers, util, id)
            env.process(bus.run())
        
        env.run(until=100)
        
        # Calculate average utilization across all buses in this simulation
        avg_util = np.mean([np.mean(util[id]) for id in util if util[id]])  # Ignore buses with no recorded utilization
        utils.append(avg_util)
    
    print(f'For {n_buses} buses:')
    print(f'Average utilization across all simulations: {round(np.mean(utils), 3)}')
    print(f'Standard error: {round(np.std(utils) / np.sqrt(n_simulations), 3)} \n')

for i in range(len(param.n_buses_list)):
    simulation(param.n_buses_list[i], param.n_simulations)