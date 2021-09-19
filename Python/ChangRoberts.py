import uuid

# possible states a process may have
IDLE = 'IDLE'
LEADER = 'LEADER'
PARTICIPANT = 'PARTICIPANT'

# possible messages
SILENT = None
ELECTION = 'ELECTION'  # + candidate.pid
RESULT = 'RESULT'  # + leader.pid

class Process:
    def __init__(self, pid = None, leader = None):
        self.pid = pid if pid is not None else uuid.uuid4().int
        self.leader = int(leader) if leader is not None else None
        self.state = IDLE
        self.inbox = (SILENT, None)
        self.outbox = (SILENT, None)

    def send(self, message, payload):
        self.outbox = (message, payload)

    def receive(self):
        (message, payload) = self.inbox
        self.inbox = (SILENT, None)
        return message, payload

    # Executes a single processing step of the Chang Roberts algorithm.
    def step(self):
        message, payload = self.receive()

        # election
        if message == ELECTION:
            candidate = int(payload)

            if candidate > self.pid:
                self.state = PARTICIPANT
                self.send(ELECTION, candidate)

            elif candidate < self.pid:
                if self.state == PARTICIPANT:
                    self.send(SILENT, None)
                else:
                    self.state = PARTICIPANT
                    self.send(ELECTION, self.pid)

            # start result propagation
            elif candidate == self.pid:
                self.state = LEADER
                self.leader = self.pid
                self.send(RESULT, self.pid)

        # results
        elif message == RESULT:
            leader = int(payload)

            if leader != self.pid:
                self.state = IDLE
                self.leader = leader
                self.send(RESULT, leader)

            else:
                self.send(SILENT, None)

        else:
            # start election
            if self.leader is None:
                self.state = PARTICIPANT
                self.send(ELECTION, self.pid)
            else:
                self.send(SILENT, None)


# possible user commands, per-process
NOOP =  '-'  # do nothing, even if not idle
STEP =  'N'  # step following the algorithm
NEW =   'E'  # start a new election
BREAK = 'X'  # make the process fail, breaking the ring

def ring_step(ring, commands):
    n = len(ring)
    assert n == len(commands)

    # fix the ring when there are one or more failures
    if BREAK in commands:
        while BREAK in commands:
            pid = commands.index(BREAK)
            commands.remove(commands[pid])
            ring.remove(ring[pid])
            for process in ring:
                if process.leader == pid:
                    process.leader = None
                if process.inbox[1] == pid:
                    process.inbox = (SILENT, None)
                if process.outbox[1] == pid:
                    process.outbox = (SILENT, None)
        return

    # otherwise, send messages around based on the provided commands
    for process, command in zip(ring, commands):
        if command == NEW:
            process.state = PARTICIPANT
            process.send(ELECTION, process.pid)
        elif command == STEP:
            process.step()
        else:
            continue

    # messages are only passed around in this last loop
    for i in range(n):
        process = ring[i]
        neighbor = ring[(i + 1) % n]
        message, payload = process.outbox
        if message != SILENT:
            neighbor.inbox = process.outbox

if __name__ == '__main__':
    # get the number of processes and create the ring
    n = int(input().strip())
    assert n >= 1
    ring = [Process(pid, leader=0) for pid in range(n)]

    try:
        while True:
            # read user inputs and drive the network
            line = input().strip()
            if line == '' or line[0] == '#':
                continue
            commands = line.upper().strip().split()
            failure = BREAK in commands
            ring_step(ring, commands)

            # print messages sent by every node
            for process in ring:
                message, payload = process.outbox
                if message:
                    print(f"(-> {message} {payload})", end=" ")
                else:
                    print("()", end=" ")
            if failure:
                print("\n# fixing ring...\n")
            else:
                print("\n")

            # print process states
            for process in ring:
                print(f"{{#{process.pid} {process.state} leader={process.leader}}}", end=" ")
            print("\n")
    except EOFError:
        pass
