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
    def __init__(self, env, routes, current_route, waiting_passengers, util, id, passenger_travel_times):
        self.env = env
        self.routes = routes
        self.current_route = current_route
        self.waiting_passengers = waiting_passengers
        self.util = util
        self.id = id
        self.current_passengers = []
        self.passenger_travel_times = passenger_travel_times

    def disembark(self):
        disembarking_passengers = []
        for passenger in self.current_passengers[:]:
            if np.random.rand() < param.q:
                disembarking_passengers.append(passenger)
        for passenger in disembarking_passengers:
            time = passenger.disembark()
            self.passenger_travel_times[self.id].append(time)
            passenger.disembark()
            self.current_passengers.remove(passenger)
            # print(passenger.__str__())
        # print(f'Disembarking: {len(disembarking_passengers)}')
        return len(disembarking_passengers)
    
    def embark(self, stop):
        waiting_passengers = len(self.waiting_passengers[stop].items)
        embarking_passengers = min(waiting_passengers, param.c - len(self.current_passengers))
        if embarking_passengers > 0:
            for _ in range(embarking_passengers):
                passenger = yield self.waiting_passengers[stop].get()
                self.current_passengers.append(passenger)
                passenger.board()
        # print(f'Embarking at stop {stop}: {embarking_passengers}')
        return embarking_passengers

    def run(self):
        while True:
            #print(f'Bus {self.id} starting route {self.current_route} at time {self.env.now}')
            stops = self.routes[self.current_route]['S']
            roads = self.routes[self.current_route]['R']

            # Loop through stops and roads
            for stop, road in zip(stops, roads):
                stop_name = stop[0] + stop[2]

                # Passengers leave the bus with probability q
                disembarking_passengers = self.disembark()

                # Passengers board the bus
                embarking_passengers = yield self.env.process(self.embark(stop))

                # Calculate utilization
                current_util = len(self.current_passengers) / param.c
                self.util[self.id].append(current_util)
                
                # Travel to next stop
                travel_time = param.travel_times[road]
                # print(f'Bus {self.id} arrives {stop_name} at time {self.env.now}. Drops off {disembarking_passengers} and picks up {embarking_passengers}. Current passengers: {len(self.current_passengers)}. Travels to next stop for {travel_time} time units.\n')
                yield self.env.timeout(travel_time)

            # print(f'Bus {self.id} has completed route {self.current_route} at time {self.env.now}')
            self.current_passengers = [] 
            self.current_route = choose_next_route(self.current_route, self.routes)
