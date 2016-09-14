open System

type Date = System.DateTime

type TaskStatus =
    | Done
    | Working
    | Todo

type Task = {
    Num     : int;
    Status  : TaskStatus;
    Text    : string;
}

type ParseTaskResult =
    | Success of Task
    | Failure

type ProgramResult =
    | ReadError
    | WriteError
    | Success
    | ParseError

(*
 *  File reading functions
 *)

let parseTask (str : string) : ParseTaskResult =
    match str with
    | 'd' :: ' ' :: task -> Success (Done task)
    | 'w' :: ' ' :: task -> Success (Working task)
    | 't' :: ' ' :: task -> Success (Todo task)
    | _ -> Failure

let unwrapTask (parseResults : ParseTaskResult list) : Task list =


let unwrapTaskList
    

let getTaskListFromFile () : Task list =
    match IO.Exists("~/.tasks") with
    | true -> seq {
                  use sr = new StreamReader ("~/.tasks")
                  while not sr.EndOfStream do
                      yield sr.ReadLine ()
              }
              |> Seq.toList
              |> List.filter (fun r -> r <> Failure)
              |> List.map (fun (Success t) -> t)
    | false -> []

(*
 *  Writing functions
 *)

let changeTask (taskList : Task list) (num : int) (status : TaskStatus) : Task list =
    let oldTask = List.nth taskList (num - 1)
    let changedTask = { Num = num; Status = status; Text = oldTask.Text }
    let rec swap 

let taskString (task : Task) : string =
    match task with
    | Done str -> "d " ++ str
    | Working str -> "w " ++ str
    | Todo str -> "t " ++ str

let writeTasks (taskList : Task list) =
    use sw = new StreamWriter ("~/.tasks")
    for t in taskList do
        sw.WriteLine (taskstring t)

(*
 *  Sorting and comparing
 *)

let taskIsDone (task : Task) =
    match task with
    | Done str -> true
    | _ -> false

let taskIsWorking (task : Task) =
    match task with
    | Working str -> true
    | _ -> false

let taskIsTodo (task : Task) =
    match task with
    | Todo str -> true
    | _ -> false

let taskSort (task1 : Task) (task2 : Task)

(*
 *  Task list query functions
 *)

let getDoneTasks (tasks : Task list) =
    List.filter taskIsDone tasks

let getWorkingTasks (tasks : Task list) =
    List.filter taskIsWorking tasks

let getTodoTasks (tasks : Task list) =
    List.filter taskIsTodo tasks

let printDoneTasks (tasks: Task list) =
    let doneTasks = getDoneTasks tasks
    for Done str in doneTasks do
        printfn "%s" str

let printWorkingTasks (tasks: Task list) =
    printfn "WORKING:"
    tasks
    |> getWorkingTasks
    |> List.map (fun t -> printfn "%A" t)

let printDoneTasks (tasks: Task list) =
    printfn "DONE:"
    tasks
    |> getDoneTasks
    |> List.map (fun t -> printfn "%A" t)

(*
 * We can:
 * Add a task as todo:          tasker add "Task to be added"
 * Delete a task:               tasker del [#]
 * Query tasks of each type     tasker [todo/working/done]
 * Query all tasks              tasker all
 * Delete our .tasks file:      tasker reset
 * Change a task status:        tasker [#] [todo/working/done]
 *)

[<EntryPoint>]
let main argv =
    let taskList = getTaskListFromFile ()
    match argv with
    | "add" :: rest  -> match rest with
                        | task :: _ -> addTask task
                        | _         -> printfn "To add a task, use: tasker add \"Task to be added\""
    | "del" :: rest  -> match rest with
                        | num :: _ when isANumber num -> deleteTask num
                        | _                           -> printfn "To delete a task, use: tasker del [#]"
    | "todo" :: _    -> taskList |> printTodoTasks
                        0
    | "working" :: _ -> taskList |> printWorkingTasks
                        0
    | "done" :: _    -> taskList |> printDoneTasks
                        0
    | "all" :: _     -> taskList |> printAllTasks
                        0
    | num :: rest    -> if isANumber num then
                            match rest with
                            | "todo" :: _    -> changeTask taskList num Todo |> writeTaskListToFile
                                                0
                            | "working" :: _ -> changeTask taskList num Working |> writeTaskListToFile
                                                0
                            | "done" :: _    -> changeTask taskList num Done |> writeTaskListToFile
                        else
                            printman
                            0
