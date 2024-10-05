import numpy as np
import simpy as sp
import param

# Function to choose intital route
def choose_init_route():
    return np.random.choice(list(param.routes.keys()))

# Function to choose next route when at last stop
def choose_next_route(current_route, routes):
    last_stop = routes[current_route]['S'][-1]  # Last stop of the current route
    next_routes = []
    for route in routes:
        if routes[route]['S'][0] == last_stop:
            next_routes.append(route)
    chosen_route = np.random.choice(next_routes)
    return chosen_route

class Bus:
    def __init__(self, env, routes, current_route, passengers, util, id):
        self.env = env
        self.routes = routes
        self.current_route = current_route
        self.passengers = passengers
        self.util = util
        self.id = id
        self.current_passengers = []

    # Function that simulates the "life cycle" of a bus
    def run(self):
        while True:
            # print(f'Bus {self.id} starting route {self.current_route} at time {self.env.now}')
            stops = self.routes[self.current_route]['S']
            roads = self.routes[self.current_route]['R']

            # Loop through stops and roads
            for stop, road in zip(stops, roads):
                
                # Passengers leave the bus with probability q
                disembarking_passengers = []
                for passenger in self.current_passengers[:]:
                    if np.random.rand() < param.q:
                        disembarking_passengers.append(passenger)
                for passenger in disembarking_passengers:
                    self.current_passengers.remove(passenger)

                # Passengers board the bus
                passengers_to_pickup = min(self.passengers[stop].level, param.c - len(self.current_passengers))
                if passengers_to_pickup > 0:
                    self.current_passengers += [1] * passengers_to_pickup
                    yield self.passengers[stop].get(passengers_to_pickup)

                # Calculate utilization
                current_util = len(self.current_passengers) / param.c
                self.util[self.id].append(current_util)
                
                # Travel to next stop
                travel_time = param.travel_times[road]
                # print(f'Bus {self.id} arrives {stop_name} at time {self.env.now}. Drops off {len(disembarking_passengers)} and picks up {passengers_to_pickup}. Current passengers: {len(self.current_passengers)}. Travels to next stop for {travel_time} time units.\n')
                yield self.env.timeout(travel_time)

            # print(f'Bus {self.id} has completed route {self.current_route} at time {self.env.now}')
            self.current_passengers = [] 
            self.current_route = choose_next_route(self.current_route, self.routes)
