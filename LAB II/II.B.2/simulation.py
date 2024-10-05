import numpy as np
import simpy as sp
import param
from bus import Bus, choose_init_route
from passenger import Passenger, passenger_generator



def simulation(n_buses, n_simulations):
    utils = []  # List to store average utilization per simulation
    passenger_travel_times = []
    for _ in range(n_simulations):
        id = 0
        env = sp.Environment()

        all_stops = set()  # Set to store all stops
        for route in param.routes.values():
            stops_in_route = route['S']
            for stop in stops_in_route:
                all_stops.add(stop)
            
        waiting_passengers = {}  # Dictionary to store passengers waiting at each stop
        for stop in all_stops:
            waiting_passengers[stop] = sp.Store(env)  # Use simpy.Store to hold passengers

        util = {}  # Utilization tracking for each bus
        for bus_id in range(n_buses):
            util[bus_id] = []
            
        travel_time = {}  # Utilization tracking for each bus
        for bus_id in range(n_buses):
            travel_time[bus_id] = []

        bus_count_per_route = {route: 0 for route in param.routes.keys()}
        
        idle_queue = sp.Store(env)

        # Start passenger generators for each stop with their respective rates
        passenger_arrival_rates = param.passenger_arrival_rates
        for stop, rate in passenger_arrival_rates.items():
            env.process(passenger_generator(id, env, str(stop), waiting_passengers[str(stop)], rate))  # Generate passengers for each stop
        
        # Start bus processes
        for id in range(n_buses):
            bus = Bus(env, param.routes, choose_init_route(), waiting_passengers, util, id, travel_time)
            env.process(bus.run(bus_count_per_route, idle_queue))
            bus_count_per_route[bus.current_route] += 1
        
        env.run(until=100) # Simulate for 100 time units

        # Calculate average utilization and travel time across all buses in this simulation
        avg_util = np.mean([np.mean(util[id]) for id in util if util[id]])
        utils.append(avg_util)
        avg_travel_time = np.mean([np.mean(travel_time[id]) for id in travel_time if travel_time[id]])
        passenger_travel_times.append(avg_travel_time)
    
    print(f'For {n_buses} buses:')
    print(f'Average utilisation across all simulations: {round(np.mean(utils), 3)}')
    print(f'Standard error for utilisation: {round(np.std(utils) / np.sqrt(n_simulations), 3)}')
    print(f'Average travel time accross all simulations: {round(np.mean(passenger_travel_times), 1)}')
    print(f'Standard error for travel: {round(np.std(passenger_travel_times) / np.sqrt(n_simulations), 3)} \n')

for i in range(len(param.n_buses_list)):
    simulation(param.n_buses_list[i], param.n_simulations)