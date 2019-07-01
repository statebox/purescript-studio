let root = ../packages.dhall

let mkPackage = root.mkPackage

let overrides = {=}

let additions = {=}

in root.packages // overrides // additions
