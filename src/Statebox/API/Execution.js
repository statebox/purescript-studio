// module Statebox.API.Execution

// TODO assume stbx.js loaded globally; warn if absent??

function fromWiring (wiring) {
  return new Stbx(wiring)
}

function fromMarked (marked) {
  return new Stbx(marked)
}

function fireOrDefault (inl) {
  return function (inr) {
    return function (s) {
      return function (tid) {
        // TODO we should be doing not inl() doen maar inl(unit)
        return s.enabled(tid) ? inr(s.fire_NON_MUTATING(tid)) : inl()
      }
    }
  }
}

function enabledOrDefault (inl) {
  return function (inr) {
    return function (s) {
      return function (tid) {
        try {
          return inr(s.enabled(tid))
        } catch (ex) {
          // console.log('enabled: error:', ex)
          // TODO we should be doing not inl() doen maar inl(unit)
          return inl()
        }
      }
    }
  }
}

function transitions (s) {
  return s.transitions()
}

function transitionCount (s) {
  return s.transitionCount()
}

function placeCount (s) {
  return s.placeCount()
}

function marking (s) {
  return s.marking()
}

// TODO handle Array index out of range
function firing (s) {
  return function (i) {
    return s.firing(i)
  }
}

module.exports = {
  fromWiring: fromWiring,
  fromMarked: fromMarked,
  gluedTransitions: transitions,
  marking: marking,
  firing: firing,
  transitionCount: transitionCount,
  placeCount: placeCount,
  fireOrDefault: fireOrDefault,
  enabledOrDefault: enabledOrDefault
};
