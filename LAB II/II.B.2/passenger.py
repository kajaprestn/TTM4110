import numpy as np

def passenger_generator(id, env, start_stop, container, passenger_arrival_rate):
    while True:
        yield env.timeout(np.random.exponential(1 / passenger_arrival_rate))
        passenger = Passenger(env, id, start_stop, container)
        id += 1
        env.process(passenger.wait())

class Passenger:

    def __init__(self, env, id, start_stop, container):
        self.env = env
        self.id = id
        self.start_stop = start_stop
        self.container = container
        self.arrival_time = 0
        self.board_time = 0
        self.disembark_time = 0
        self.total_time = 0

    def wait(self):
        self.arrival_time = self.env.now
        yield self.container.put(self)

    def board(self):
        self.board_time = self.env.now

    def disembark(self):
        self.disembark_time = self.env.now
        self.total_time = self.disembark_time - self.arrival_time
        return self.total_time

    def __str__(self):
        return f'ID: {self.id} start_stop: {self.start_stop} Arrival time: {self.arrival_time}, Board time: {self.board_time}, Disembark time: {self.disembark_time}, Travel time: {self.total_time}'
    
    