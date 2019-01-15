// module Svg.Util

exports._beginElements = function (cssSelectorStr) {
  return function (onError, onSuccess) {
    console.log('beginElementsAff: cssSelectorStr =', cssSelectorStr);
    const elems = document.querySelectorAll(cssSelectorStr);
    console.log('beginElementsAff: elems =', elems);
    const numElems = elems.length || -1;
    console.log('beginElementsAff: number of elements found: ', numElems);

    elems.forEach(function (elem) {
      elem.beginElement();
    });

    // TODO onError case?
    onSuccess(numElems);

    return function (cancelError, cancelerError, cancelerSuccess) {
      cancelerSuccess(); // no cleanup to do
    };
  };
}

var svg;

exports.domToSvgCoordinates = function (point) {
  if (typeof svg === 'undefined') {
    svg = document.getElementsByTagName('svg')[0];
  }

  const svgPoint = svg.createSVGPoint();

  svgPoint.x = point.value0;
  svgPoint.y = point.value1;

  const svgCoordPoint = svgPoint.matrixTransform(svg.getScreenCTM().inverse());

  return {
    value0: svgCoordPoint.x,
    value1: svgCoordPoint.y
  };
}
