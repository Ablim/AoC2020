module Validators

open BoardingPass

let isValidBoardingPass pass row col id =
    let r = getRow pass
    let c = getColumn pass
    let i = getId pass
    r = row && c = col && i = id