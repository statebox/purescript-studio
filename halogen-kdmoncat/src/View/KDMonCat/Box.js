exports.getBBox = function (el) {
  return function () {
    var rect = el.getBBox();
    return {
      x: rect.x,
      y: rect.y,
      width: rect.width,
      height: rect.height
    };
  };
};
