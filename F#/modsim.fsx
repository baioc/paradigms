#r "nuget: MathNet.Numerics.FSharp"
#r "nuget: FSharpx.Collections"

open MathNet.Numerics.Distributions
open FSharpx.Collections


type ProcessKind = System | User
type Process = { Kind: ProcessKind; ArrivalTime: float }
type Event = Arrival | Terminate of Process
type Calendar = Heap<float * Event>
type ResourceQueue = { Available: int; Queue: Queue<Process> }
type TraceElement =
    | State of ResourceQueue * ResourceQueue
    | Event of float * Event
    | Create of Process


let timeLimit = 500.0

let arrivalDist = Exponential(1.0)
let executionDist = Normal(4.0, 1.0)
let systemDist = Bernoulli(0.1)


let takeCPU cpu proc t =
    let terminateEvent = (t + executionDist.Sample(), Terminate proc)
    { cpu with Available = cpu.Available - 1 }, terminateEvent

let waitCPU cpu proc =
    { cpu with Queue = Queue.conj proc cpu.Queue }

let dropCPU cpu =
    { cpu with Available = cpu.Available + 1 }

let giveCPU cpu t =
    let next, q = Queue.uncons cpu.Queue
    let terminateEvent = (t + executionDist.Sample(), Terminate next)
    { cpu with Queue = q }, terminateEvent

let rec simulate (currentTime: float) (calendar: Calendar) (sysQueue: ResourceQueue) (userQueue: ResourceQueue) = seq {
    if currentTime > timeLimit || Heap.isEmpty calendar then
        ()
    else
        yield State (sysQueue, userQueue)
        let (currentTime, event), calendar = Heap.uncons calendar
        yield Event (currentTime, event)

        match event with
        | Arrival ->
            let kind = if systemDist.Sample() = 1 then System else User
            let proc = { Kind = kind; ArrivalTime = currentTime }
            yield Create proc
            match proc.Kind with
            | System ->
                if sysQueue.Available > 0 then
                    let sysQueue, event = takeCPU sysQueue proc currentTime
                    yield! simulate currentTime (Heap.insert event calendar) sysQueue userQueue
                else
                    let sysQueue = waitCPU sysQueue proc
                    yield! simulate currentTime calendar sysQueue userQueue
            | User ->
                if userQueue.Available > 0 then
                    let userQueue, event = takeCPU userQueue proc currentTime
                    yield! simulate currentTime (Heap.insert event calendar) sysQueue userQueue
                else
                    let userQueue = waitCPU userQueue proc
                    yield! simulate currentTime calendar sysQueue userQueue
        | Terminate proc ->
            match proc.Kind with
            | System ->
                if Queue.isEmpty sysQueue.Queue then
                    let sysQueue = dropCPU sysQueue
                    yield! simulate currentTime calendar sysQueue userQueue
                else
                    let sysQueue, event = giveCPU sysQueue currentTime
                    yield! simulate currentTime (Heap.insert event calendar) sysQueue userQueue
            | User ->
                if Queue.isEmpty userQueue.Queue then
                    let userQueue = dropCPU userQueue
                    yield! simulate currentTime calendar sysQueue userQueue
                else
                    let userQueue, event = giveCPU userQueue currentTime
                    yield! simulate currentTime (Heap.insert event calendar) sysQueue userQueue
}


let replications = 100


let timings = [
    let initialCalendar =
        (0.0, arrivalDist.Samples())
        ||> Seq.scan (fun t dt -> t + dt)
        |> Seq.takeWhile (fun t -> t <= timeLimit)
        |> Seq.map (fun t -> t, Arrival)
        |> Heap.ofSeq false

    let sysQueue = { Available = 1; Queue = Queue.empty }
    let userQueue = { Available = 3; Queue = Queue.empty }

    for _ in 1 .. replications do
        let avgTime =
            simulate 0.0 initialCalendar sysQueue userQueue
            |> Seq.choose
                (function
                    | Event (t, Terminate proc) when proc.Kind = System -> Some (t - proc.ArrivalTime)
                    | other -> None)
            |> Seq.average
        yield avgTime
]

let n = List.length timings
let avg = Seq.average timings
let std =
    timings
    |> Seq.map (fun x -> (x - avg)**2)
    |> Seq.fold (+) 0.0
    |> fun ssd -> sqrt (ssd / float(n - 1))
let z95 = 1.96
let halfInterval = z95 * std / (sqrt <| float n)
let min, max = avg - halfInterval, avg + halfInterval
printfn "%g, %g" min max
