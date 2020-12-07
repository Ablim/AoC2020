module CustomCustoms

open Common.Common

let distinctQuestions (questions : string list) =
    merge questions |>
        Seq.toList |>
            Seq.distinct |>
                Seq.toList |>
                    listLength

let getQuestionSum questions =
    splitIntoGroups questions |>
        Seq.map (fun x -> distinctQuestions x) |>
            Seq.sum

let answeredByAll (questions : string list) =
    let groupSize = questions.Length
    let allAnswers = merge questions |> Seq.toList
    let answerGroups = Seq.countBy (fun x -> x) allAnswers
    Seq.map (fun (_, v) -> if v = groupSize then 1 else 0) answerGroups |>
            Seq.sum

let getQuestionSum2 questions =
    splitIntoGroups questions |>
        Seq.map (fun x -> answeredByAll x) |>
            Seq.sum