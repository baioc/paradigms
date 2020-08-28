#light "off"

open System.Text.RegularExpressions
open System.Net
open Microsoft.FSharp.Core
open FSharp.Data.UnitSystems


let download (url: string) =
    (new WebClient()).DownloadString(url);;

let extractLinks html =
    Regex.Matches(html, @"http://\S+");;

let downloadAndExtractLinks url =
    let links = url |> download |> extractLinks in url, links.Count;;


// Seq module extension
module Seq = begin
    let pmap f list =
        seq { for a in list -> async { return f a } }
        |> Async.Parallel
        |> Async.RunSynchronously;;
end;;



// [<Measure>] type ms;;

let timed f =
    fun x ->
        let stopwatch = System.Diagnostics.Stopwatch.StartNew() in
        let result = f x in
        stopwatch.Stop();
        ( result, (LanguagePrimitives.FloatWithMeasure
                    stopwatch.Elapsed.TotalMilliseconds * 1e-3: float<SI.UnitSymbols.s>) );;


let testSync urls =
    timed (List.map downloadAndExtractLinks) <| urls;;

let testAsync urls =
    timed (Seq.pmap downloadAndExtractLinks) <| urls;;

let urls =
    [
        @"https://fsharp.org/";
        @"https://docs.microsoft.com/en-us/dotnet/fsharp/";
        @"https://fsharp.github.io/fsharp-core-docs/";
        @"https://fsharpforfunandprofit.com/";
        @"https://en.wikibooks.org/wiki/F_Sharp_Programming";
    ];;

[<EntryPoint>]
let main argv =
    let urls = if Array.length argv > 0 then Array.toList argv else urls in
    let printTimed msg f x =
        let result, time = f x in
        printfn "(%f s) %s: %A" time msg result in
    begin
        printfn "Start...";
        printTimed "Synchronous" testSync urls;
        printTimed "Asynchronous" testAsync urls;
        printfn "Done.";
    end;
    0;;
