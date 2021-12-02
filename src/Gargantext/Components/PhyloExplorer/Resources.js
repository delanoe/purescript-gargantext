'use strict';

////////////////////////////////////////////////////////////////////////////////
///    FIELDS
////////////////////////////////////////////////////////////////////////////////

var EXTRACTED_TERMS_EVENT     = 'extracted_terms_event';
var EXTRACTED_COUNT_EVENT     = 'extracted_count_event';
var SELECTED_TERM_EVENT       = 'selected_term_event';
var SELECTED_BRANCH_EVENT     = 'selected_branch_event';
var SELECTED_SOURCE_EVENT     = 'selected_source_event';
var DISPLAY_VIEW_EVENT        = 'display_view_event';

var ISO_LINE_DOM_QUERY      = '.phylo-isoline';
var LEFT_COLUMN_DOM_QUERY   = '.phylo-grid__blueprint__left';
var CENTER_COLUMN_DOM_QUERY = '.phylo-grid__blueprint__center';
var SCAPE_DOM_QUERY         = '.phylo-grid__content__scape';

//  (?) Global thread dependencies:
//    * d3 <Object> (main D3 proxy))
//    * window <Window> (cf. below function signature for window uses)
//    * document <HTMLDocument>

var memoTickText          = {};        // <Object> of <Int> => <TickText>
///   <TickText> ::
///       <Int> bId
///       <Float> limit
///       <String>text
var panel                 = undefined; // <Object> instanceof d3.selection
var svg                   = undefined; // <Object> instanceof d3.selection
var label                 = undefined; // <Object> instanceof d3.selection
var zoom                  = undefined; // <Function> see https://github.com/d3/d3-zoom#zoom
var xScale0               = undefined; // <Function> see https://github.com/d3/d3-scale#_continuous
var yScale0               = undefined; // <Function> see https://github.com/d3/d3-scale#_continuous
var subscribers           = {};        // <Object> dictionary for pubsub pattern

////////////////////////////////////////////////////////////////////////////////
///    HELPERS
////////////////////////////////////////////////////////////////////////////////

/**
 * @name contains
 * @param {String} str
 * @param {Array} arr
 * @returns {Boolean}
 */
function contains(str,arr) {
  return arr.indexOf(str) > -1;
}
/**
 * @name addDays
 * @param {String|Date} date
 * @param {Int} days
 * @returns {Date}
 */
function addDays(date, days) {
  var result = new Date(date);
  result.setDate(result.getDate() + days);
  return result;
}
/**
 * @name removeDays
 * @param {String|Date} date
 * @param {Int} days
 * @returns {Date}
 */
function removeDays(date, days) {
  var result = new Date(date);
  result.setDate(result.getDate() - days);
  return result;
}
/**
 * @name appendCSS
 * @param {String} cssText
 * @param {Element} element
 * @unpure {HTMLDocument} document
 */
function appendCSS( cssText, element ) {
  var styleElement = document.createElement("style");
  styleElement.setAttribute("type","text/css");
  styleElement.innerHTML = cssText;
  var refNode = element.hasChildNodes() ? element.children[0] : null;
  element.insertBefore( styleElement, refNode );
}
/**
 * @name debounce
 * @param {Function<*>} fn
 * @param {Int} wait
 * @param {Boolean} immediate [default: undefined]
 * @returns {Function<*>}
 */
function debounce(fn, wait, immediate) {
  var timeout;
  return function() {
    var context = this
    , args = arguments
    , later = function() {
      timeout = null;
      if (immediate !== true) {
        fn.apply(context, args);
      }
    }
    , now = immediate === true && timeout === null;

    clearTimeout(timeout);
    timeout = setTimeout(later, wait);

    if (now === true) {
      fn.apply(context, args);
    }
  }
}
/**
 * @name rdm
 * @returns {Int}
 */
function rdm() {
  if (Math.random() > 0.5) {
    return 1;
  } else {
    return -1;
  }
}
/**
 * @name arraySum
 * @param {*} acc
 * @param {*} curr
 */
function arraySum(acc, curr) {
  return acc + curr
}
/**
 * (?) Use of a PubSub pattern has been empiracally implemented to provide a
 *     behavorial interface linking PureScript and JavaScript processes
 *
 *     The PubSub will be used for bridge communication with functions (such
 *     as `drawWordCloud`, `termClick`, etc.) that can both perform as:
 *        -              JavaScript → PureScript
 *        - PureScript → JavaScript → PureScript
 *
 *     One ideal solution would have been to translate this very JavaScript
 *     module in PureScript, but due to a time-consuming issue certain parts are
 *     still in JavaScript
 *
 *     For these reasons, we decided to use a PubSub with event from JavaScript
 *     ↔ PureScript. It is a simpler version as Justin Woo made in its repo [1]
 *     It however make an assumption that every `callback` provided from a new
 *     subscription was provided in a PureScript `Effect` thunk
 *
 *
 * @name pubsub
 * @param {Object} subscribers of <Array> of <Object> dictionary
 *    <String> id => <Function<*>> callback
 * @pattern module
 * @link https://github.com/justinwoo/call-ps-from-js [1]
 */
var pubsub = (function(subscribers) {
  /**
   * @name generateUUID
   * @access private
   * @returns {String}
   */
  function generateUUID() {
    return ([1e7]+-1e3+-4e3+-8e3+-1e11).replace(/[018]/g, c =>
      (c ^ crypto.getRandomValues(new Uint8Array(1))[0] & 15 >> c / 4).toString(16)
    );
  }

  return {
    /**
     * @name subscribe
     * @access public
     * @param {String} eventLabel
     * @param {Function<*>} cbk
     * @returns {String} subscriptionId
     */
    subscribe(eventLabel, cbk) {
      var id;

      if (subscribers.hasOwnProperty(eventLabel) === false) {
        subscribers[eventLabel] = {};
      }

      id = generateUUID();

      subscribers[eventLabel][id] = cbk;

      return id;
    },
    /**
     * @name publish
     * @access public
     * @param {String} eventLabel
     * @param {*} data
     */
    publish(eventLabel, data) {
      if (subscribers.hasOwnProperty(eventLabel) === false) {
        return;
      }

      Object.keys(subscribers[eventLabel]).forEach(
        function(subscriptionId) {
          // assuming it came from PureScript (ie. as an `Effect`)
          return subscribers[eventLabel][subscriptionId](data)();
        }
      );
    },
    /**
     * @name unsubscribe
     * @access public
     * @param {String} eventLabel
     * @param {String} subscriptionId
     */
    unsubscribe(eventLabel, subscriptionId) {
      if (subscribers.hasOwnProperty(eventLabel) === false) {
        return;
      }

      if (subscribers[eventLabel].hasOwnProperty(subscriptionId) === false) {
        return;
      }

      delete subscribers[eventLabel][subscriptionId];
    }
  };
})(subscribers);

////////////////////////////////////////////////////////////////////////////////
///    ACTIONS
////////////////////////////////////////////////////////////////////////////////

/**
 * @name groupTermsBy
 * @param {HTMLCollection} elements
 * @param {String} attr
 * @returns {Array}
 *    <Array>
 *        <String> stringified float
 *        <String> stringified float
 *        <String> stringified int
 *    <Array>
 *        <String> stringified float
 *        <String> stringified float
 *        <String> stringified int
 */
function groupTermsBy(elements, attr) {
  let grouped = {},
      curr = "";
  for (var i = 0; i < elements.length; i++) {
    let from = elements[i].getAttribute(attr)
    if (curr != from) {
      grouped[from] = [[(elements[i]).getAttribute("gx"),(elements[i]).getAttribute("gy"),(elements[i]).getAttribute("bid")]];
      curr = from
    } else {
      grouped[from].push([(elements[i]).getAttribute("gx"),(elements[i]).getAttribute("gy"),(elements[i]).getAttribute("bid")]);
    }
  }
  return Object.values(grouped);
}
/**
 * @name showLabel
 * @unpure {HTMLDocument} document
 */
function showLabel() {
  var ngrams = document.getElementsByClassName("ngrams");
  var groups = document.getElementsByClassName("group-inner");
  var headers = document.getElementsByClassName("header");

  window.ldView = false;

  d3.selectAll(".group-path")
    .classed("path-heading", false);

  Array.from(groups).forEach(function(item) {
    item.style.fill = "#fff";
    item.classList.remove("group-heading");
  })

  Array.from(headers).forEach(function(item) {
    item.style.visibility = "hidden";
  })

  Array.from(ngrams).forEach(function(item) {
    item.style.visibility = "visible";
    item.style.fill = '#0d1824';
  })
}
/**
 * @name showHeading
 * @unpure {Window.<Boolean>} window.ldView
 * @unpure {Object} d3
 * @unpure {HTMLDocument} document
 */
function showHeading() {
  var ngrams = document.getElementsByClassName("ngrams");
  var groups = document.getElementsByClassName("group-inner");
  var headers = document.getElementsByClassName("header");

  window.ldView = true;

  d3.selectAll(".group-path")
    .classed("path-heading", true);

  Array.from(groups).forEach(function(item) {
    item.style.fill = "#f5eee6";
    item.classList.add("group-heading");
  })

  Array.from(headers).forEach(function(item) {
    item.style.visibility = "visible";
  })

  Array.from(ngrams).forEach(function(item) {
    item.style.visibility = "hidden";
  })
}
/**
 * @name showLanding
 * @unpure {Window.<Boolean>} window.ldView
 * @unpure {Object} d3
 * @unpure {HTMLDocument} document
 */
function showLanding() {
  var ngrams = document.getElementsByClassName("ngrams");
  var groups = document.getElementsByClassName("group-inner");
  var headers = document.getElementsByClassName("header")

  window.ldView = true;

  d3.selectAll(".group-path")
    .classed("path-heading", false);

  Array.from(groups).forEach(function(item) {
    item.style.fill = "#61a3a9";
    item.classList.remove("group-heading");
  })

  Array.from(headers).forEach(function(item) {
    item.style.visibility = "hidden"
  })

  Array.from(ngrams).forEach(function(item) {
    item.style.fill = "#61a3a9";
    item.style.visibility = "hidden";
  })
}
/**
 * @name resetSelection
 * @unpure {Window.Array.<Int>} window.branchFocus
 * @unpure {Object} pubsub
 */
function resetSelection() {
  window.branchFocus = [];
  pubsub.publish(SELECTED_TERM_EVENT, '');
  pubsub.publish(SELECTED_BRANCH_EVENT, '');
  pubsub.publish(SELECTED_SOURCE_EVENT, '');
  pubsub.publish(EXTRACTED_TERMS_EVENT, []);
  pubsub.publish(EXTRACTED_COUNT_EVENT, '');
}
/**
 * @name doubleClick
 * @unpure {Window.<Boolean>} window.highlighted
 * @unpure {Object} d3
 * @unpure {Object} pubsub
 */
function doubleClick() {
  window.highlighted = false;
  headerOut();
  d3.selectAll(".group-inner")
    .classed("group-unfocus",false)
    .classed("group-focus",false);
  d3.selectAll(".group-path")
    .classed("path-unfocus",false)
    .classed("path-focus",false);
  d3.selectAll(".term-path").remove();
  d3.selectAll(".peak").classed("peak-focus",false);
  d3.selectAll(".peak").classed("peak-focus-source",false);
  d3.selectAll(".x-mark").style("fill","#4A5C70");
  resetSelection();
}
/**
 * @name headerOut
 * @unpure {Object} d3
 */
function headerOut() {
  d3.selectAll(".header").nodes().forEach(function(header){
    header.style["font-size"] = header.getAttribute("mem-size") + "px";
    header.style["opacity"] = header.getAttribute("mem-opac");
  })
}
/**
 * @name termClick
 * @param {String} txt
 * @param {String} idx stringified int
 * @param {Int} nodeId
 * @param {String} typeNode "group|head|search"
 * @unpure {Object} d3
 * @unpure {HTMLDocument} document
 * @unpure {Window.Array.<Int>} window.branchFocus
 * @unpure {Object} panel instanceof d3.selection
 * @unpure {Object} pubsub
 */
function termClick (txt,idx,nodeId,typeNode) {
  // remove old focus
  initPath()
  resetSelection();

  // catch the last transformations
  if (typeNode == "group") {
    var transform = d3.select("#group" + nodeId).node().getAttribute("transform");
  } else if (typeNode == "head") {
    var transform = d3.select("#head" + nodeId).node().getAttribute("transform");
  } else {
    var transform = (d3.selectAll(".header").nodes())[0].getAttribute("transform");
  }

  // focus

  pubsub.publish(SELECTED_TERM_EVENT, txt);

  // highlight the groups

  var terms = document.getElementsByClassName("fdt-" + idx),
      periods = groupTermsBy(terms,"from");

  var groups  = [];

  for (var i = 0; i < terms.length; i++) {
    groups.push(d3.select("#group" + (terms[i]).getAttribute("gid")));
    window.branchFocus.push(
      parseInt( (terms[i]).getAttribute("bid"), 10 )
    );
  }

  highlightGroups(groups.map(g => g.node()));
  drawWordCloud(groups.map(g => g.node()));

  // highlight the cross branches links

  var bids  = [];

  for (var i = 0; i < periods.length; i++) {
    if (i != periods.length - 1) {
      for (var j = 0; j < periods[i].length; j++) {
        bids.push(periods[i][j][2])
        var x1 = periods[i][j][0],
            y1 = periods[i][j][1];
        for (var k = 0; k < periods[i + 1].length; k++) {
          var x2 = periods[i + 1][k][0],
              y2 = periods[i + 1][k][1];
          if ((periods[i][j][2] != periods[i + 1][k][2]) && (!bids.includes(periods[i + 1][k][2]))) {
            // draw the links between branches
            panel
              .append("path")
              .attr("class","term-path")
              .attr("d", function(d) {
                return "M" + x1 + "," + y1
                  + "C" + x2 + "," + y1
                  + " " + x2 + "," + y2
                  + " " + x2 + "," + y2;
              })
              .attr("transform",transform)
              .style("stroke-opacity", 0.4)
              .lower();
          }
          bids.push(periods[i + 1][k][2])
        }
      }
    }
  }

  d3.selectAll(".path-unfocus").lower();
}
/**
 * @name initPath
 * @unpure {Object} d3
 */
function initPath () {
  let groups = d3.selectAll(".group-inner");
  (groups.nodes()).map(function(g){
    if (!g.classList.contains("source-focus")) {
      g.classList.add("group-unfocus");
      g.classList.remove("group-focus");
    }
  })
  d3.selectAll(".group-path")
    .classed("path-unfocus",true)
    .classed("path-focus",false);
  d3.selectAll(".term-path").remove();
  d3.selectAll(".peak").classed("peak-focus",false);
  d3.selectAll(".peak").classed("peak-focus-source",false);
  d3.selectAll(".x-mark").style("fill","#4A5C70");
}
/**
 * @name highlightGroups
 * @param {Array.<Element>} groups
 * @unpure {HTMLDocument} document
 * @unpure {Object} d3
 * @unpure {Object} pubsub
 */
function highlightGroups(groups) {

  let paths = document.getElementsByClassName("group-path"),
      gids  = [];

  for (var i = 0; i < groups.length; i++) {

    // highlight the groups

    groups[i]
      .classList.add("group-focus");
    groups[i]
      .classList.remove("group-unfocus");
      // .classed("group-unfocus", false)
      // .classed("group-focus", true);

    gids.push(groups[i].getAttribute("gid"))

    // highlight the branches peak

    let bid = groups[i].getAttribute("bId")

    d3.select("#peak-" + bid)
      .classed("peak-focus", true);
    d3.select("#xmark-" + bid)
      .style("fill", "#F0684D");

  }

  // facets

  var count = {
    groupCount: groups.length,
    termCount: countTerms(groups),
    branchCount: countBranches(groups)
  };

  pubsub.publish(
    EXTRACTED_COUNT_EVENT,
    JSON.stringify( count )
  );

  // highlight the links

  for (var i = 0; i < paths.length; i++) {
    if (gids.includes((paths[i]).getAttribute("source")) && (paths[i]).getAttribute("target")) {
      paths[i].classList.add("path-focus");
      paths[i].classList.remove("path-unfocus");
    }
  }
}
/**
 * @name countTerms
 * @param {Array.<SVGCircleElement>} groups
 * @unpure {Object} d3
 * @returns {Int}
 */
function countTerms(groups) {
  var terms = [];
  for (var i = 0; i < groups.length; i++) {
    let gid = ((groups[i].getAttribute("id")).split("group"))[1]
    d3.selectAll(".g-" + gid).nodes().forEach(e => terms.push(e.getAttribute("fdt")))
  }
  return (Array.from(new Set(terms))).length;
}
/**
 * @name countBranches
 * @param {Array.<SVGCircleElement>} groups
 * @returns {Int}
 */
function countBranches(groups) {
  var branches = [];
  for (var i = 0; i < groups.length; i++) {
    branches.push(groups[i].getAttribute("bId"));
  }
  return (Array.from(new Set(branches))).length;
}
/**
 * @name resetView
 * @unpure {Object} svg instanceof d3.selection
 */
function resetView() {
  svg.transition()
      .duration(750)
      .call(zoom.transform, d3.zoomIdentity);
}
/**
 * @name peakClick
 * @param {Object} b <Gargantext.Components.PhyloExplorer.Types.Branch>
 * @param {Object} coordinates
 *    <Float> x
 *    <Float> y
 *    <Float> w
 *    <Float> h
 * @unpure {Object} d3
 * @unpure {Function} zoom
 * @unpure {Window.Array.<Int>} window.branchFocus
 */
function peakClick (b, coordinates) {
  let groups = d3.selectAll(".group-inner").filter(".branch-" + b.bId).nodes();

  initPath();
  resetSelection();

  pubsub.publish(SELECTED_BRANCH_EVENT, b.label);
  window.branchFocus.push( parseInt(b.bId, 10) );

  drawWordCloud(groups);
  highlightGroups(groups);

  /* rescale */
  let tx = (groups[0]).getAttribute("cx")
  svg.transition()
      .duration(750)
      .call(zoom.transform, d3.zoomIdentity.translate(((coordinates.x + coordinates.w) / 2) - tx, 0).scale(1));

  d3.selectAll(".path-unfocus").lower();
}
/**
 * @name peakOver
 * @param {Object} b <Gargantext.Components.PhyloExplorer.Types.Branch>
 * @param {Int} i
 * @unpure {Object} d3
 * @unpure {Object} label
 * @unpure {Function<Int>} yScale0 see https://github.com/d3/d3-scale#_continuous
 * @unpure {Function<Int>} xScale0 see https://github.com/d3/d3-scale#_continuous
 */
function peakOver (b,i) {
  d3.select("#peak-" + i).classed("peak-focus",false);
  d3.select("#peak-" + i).classed("peak-over",true);
  label.text(b.label.replace(/"/g,''))
       .style("visibility", "visible")
       .style("top", yScale0(b.y) + "px")
       .style("left",xScale0(b.x1) + "px");
  branchOver(b.bId);
}
/**
 * @name peakOut
 * @param {Object} b <Gargantext.Components.PhyloExplorer.Types.Branch>
 * @param {Int} i
 * @unpure {Object} d3
 * @unpure {Window.Array<Int>} window.branchFocus
 */
function peakOut (b,i) {
  d3.select("#peak-" + i).classed("peak-over",false);
  if (window.branchFocus.includes(b.bId)) {
    d3.select("#peak-" + i).classed("peak-focus",true);
  }
  branchOut();
}
/**
 * @name showPeak
 * @unpure {Object} d3
 */
function showPeak() {
  var centerColumnCoordinates = getCenterColumnCoordinates();

  var centerColumn = d3
    .select(CENTER_COLUMN_DOM_QUERY)
    .node()
    .getBoundingClientRect();

  var xAxis = d3
    .select(".x-axis")
    .node()
    .getBoundingClientRect();

  var xBounds = [
    centerColumn.left,
    centerColumn.left
      // (?) as we cannot rely on the created "scape" SVG width (which results
      //     may vary on height, due to coordinate matrix ratio preserving),
      //     we have to find the real SVG width via a trick: here we'll relying //     on the xAxis
      + xAxis.width
      // add to be added due to use of `xAxis.width` trim padding
      + centerColumnCoordinates.l
      + centerColumnCoordinates.r
  ];

  var yBounds = [
    centerColumn.top,
    centerColumn.top + centerColumn.height
  ];

  d3
    .selectAll(".peak")
    .style(
      "fill"
    , function(peak,i) {
        var isVisible = d3
          .selectAll(".branch-" + i)
          .nodes()
          .map(function(g){
            var rect = g.getBoundingClientRect();

            var x = rect.x,
                y = rect.y;
            // Adjustment values empirically managing intersection
            var dx =
                  //  + centerColumnCoordinates.l
                  //  - centerColumnCoordinates.r
                   - rect.width
            var dy =
                   + centerColumnCoordinates.t
                   - centerColumnCoordinates.b
                   - rect.height
            // Enclosure + Intersection
            var xLower = x >= (xBounds[0] + dx);
            var xUpper = x <=  xBounds[1];

            var yLower = y >= (yBounds[0] + dy);
            var yUpper = y <=  yBounds[1];

            return xLower && xUpper && yLower && yUpper;
          })
          .reduce((mem,cur) => {return mem || cur;})

        if (isVisible) {
            d3.select("#peak-shadow" + i).attr("visibility","visible");
            return "#0d1824";
        } else {
            d3.select("#peak-shadow" + i).attr("visibility","hidden");
            return "#A9A9A9";
        }
    }
  );
}
/**
 * @name branchOver
 * @param {Int} bId
 * @unpure {Window.<String>} window.displayView
 * @unpure {Object} d3
 */
function branchOver(bId) {
  // headers
  if (window.displayView === "headingMode") {
    d3.selectAll(".header").nodes().forEach(function(header){
      if (header.getAttribute("bid") == bId) {
        header.style["font-size"] = "10px";
        header.style["opacity"] = 1;
      } else {
        header.style["opacity"] = 0.3;
      }
    })
  }
  // branches
  d3.select("#xmark-"  + bId).style("fill","#f3be54");
  d3.select("#hover-" + bId).style("visibility","visible");
}
/**
 * @name branchOut
 * @param {Int} bId
 * @unpure {Object} d3
 * @unpure {Window.Array<Int>} window.branchFocus
 */
function branchOut() {
  d3.selectAll(".peak-label").style("visibility","hidden");
  d3.selectAll(".branch-hover").style("visibility","hidden");
  d3.selectAll(".x-mark").style("fill","#4A5C70");
  for (var i = 0; i < window.branchFocus.length; i++) {
    d3.select("#xmark-" + window.branchFocus[i]).style("fill","#F24C3D");
  }
  headerOut();
}
/**
 * @name tickClick
 * @param {Array} branches of <Gargantext.Components.PhyloExplorer.Types.Branch>
 * @param {Element} tick
 * @unpure {Object} d3
 * @unpure {Window.Array<Int>} window.branchFocus
 * @unpure {Object} pubsub
 */
function tickClick(branches, tick) {
  let bid = parseInt(tick.getAttribute("bId"), 10),
      groups = d3.selectAll(".group-inner").filter(".branch-" + bid).nodes(),
      branch = branches.find(function(item) {
        return item.bId === bid;
      });

  initPath();
  resetSelection();

  pubsub.publish(SELECTED_BRANCH_EVENT, branch.label);
  window.branchFocus.push(bid);

  drawWordCloud(groups);

  highlightGroups(groups);
  d3.selectAll(".path-unfocus").lower();
}
/**
 * @name tickOver
 * @param {Element} tick
 * @param {Array} branches of <Gargantext.Components.PhyloExplorer.Types.Branch>
 * @unpure {Object} d3
 */
function tickOver(tick, branches) {
  var ego = tick.getAttribute("bId"),
      branch = branches.find(b => b.bId == ego);
  if (d3.select("#peak-" + ego).node().style.visibility != "hidden") {
      branchOver(ego);
      peakOver(branch,ego);
  }
}
/**
 * @name tickOut
 * @param {Element} tick
 * @param {Array} branches of <Gargantext.Components.PhyloExplorer.Types.Branch>
 */
function tickOut(tick, branches) {
  var ego = tick.getAttribute("bId"),
      branch = branches.find(b => b.bId == ego);
  branchOut();
  peakOut(branch,ego)
}
/**
 * @name onZoom
 * @param {Event} event
 * @param {Function<Int>} xScale see https://github.com/d3/d3-scale#_continuous
 * @param {Function<Int>} yScale see https://github.com/d3/d3-scale#_continuous
 * @param {Array<Object>} xLabels
 *    <Float> x
 *    <String> label
 *    <Float> inf
 *    <Float> sup
 *    <Int> bId
 * @param {Array<Object>} yLabels
 *    <Date> from
 *    <Int> label
 *    <Date> to
 *    <Float> y
 * @param {Array} branches of <Gargantext.Components.PhyloExplorer.Types.Branch>
 * @param {Object} xAxis instanceof d3.selection
 * @param {Object} yAxis instanceof d3.selection
 * @unpure {Object} panel
 */
function onZoom(event, coordinates, xScale, yScale, xLabels, yLabels, branches, xAxis, yAxis) {
  var xBounds = coordinatesToXRange(coordinates);
  var yBounds = coordinatesToYRange(coordinates);

  var zoomX = event.transform.rescaleX(xScale),
      zoomY = event.transform.rescaleY(yScale),
      zoomXLabels = xLabels.filter(
        function(b) {
          var lower = zoomX(b.x) >= xBounds[0];
          var upper = zoomX(b.x) <= xBounds[1];

          return lower && upper;
        }
      ),
      zoomYLabels = yLabels.filter(
        function(p) {
          var lower = zoomY(p.y) >= yBounds[0];
          var upper = zoomY(p.y) <= yBounds[1];

          return lower && upper;
        }
      );

  setAxisX(zoomX, zoomXLabels, branches, xAxis);
  setAxisY(zoomY, zoomYLabels, yAxis);

  panel.selectAll("circle").attr("transform", event.transform);
  panel.selectAll("text").attr("transform", event.transform);
  panel.selectAll("path").attr("transform", event.transform);
  panel.selectAll(".branch-hover").attr("transform", event.transform);
  panel.selectAll(".y-highlight").attr("transform", event.transform);
  panel.selectAll(".ngrams").attr("transform", event.transform);
  panel.selectAll(".term-path").attr("transform", event.transform);
  panel.selectAll(".emergence").attr("transform", event.transform);
  panel.selectAll(".header").attr("transform", event.transform);
  panel.selectAll(".header-wrapper").attr("transform", event.transform);

  showPeak();
}

// function groupOver() {
//     var from = this.getAttribute("from");
//     d3.select("#y-highlight-" + from).style("visibility","visible");
//     // d3.select("#y-mark-year-inner-" + from).node().setAttribute("class","y-mark-year-inner-highlight");
//     // d3.select("#y-mark-year-outer-" + from).node().setAttribute("class","y-mark-year-outer-highlight");
//     // d3.select("#y-label-" + from).node().setAttribute("class","y-label-bold");
// }

// function groupOut() {
//     var from = this.getAttribute("from");
//     d3.select("#y-highlight-" + from).style("visibility","hidden");
//     // d3.select("#y-mark-year-inner-" + from).node().setAttribute("class","y-mark-year-inner");
//     // d3.select("#y-mark-year-outer-" + from).node().setAttribute("class","y-mark-year-outer");
//     // d3.select("#y-label-" + from).node().setAttribute("class","y-label");
// }

////////////////////////////////////////////////////////////////////////////////
///    EXPORTS
////////////////////////////////////////////////////////////////////////////////

/**
 * @name exportViz
 */
function exportViz() {
  var time = new Date();

  serialize(svg.node(),"phylomemy-" + Date.parse(time.toString())  + ".svg")
}
/**
 * @name serialize
 * @param {SVGSVGElement} graph
 * @param {String} name
 * @unpure {Window} window
 * @unpure {HTMLDocument} document
 */
function serialize(graph,name) {
  const xmlns = "http://www.w3.org/2000/xmlns/";
  const xlinkns = "http://www.w3.org/1999/xlink";
  const svgns = "http://www.w3.org/2000/svg";

  graph = graph.cloneNode(true);
  const fragment = window.location.href + "#";
  const walker = document.createTreeWalker(graph, NodeFilter.SHOW_ELEMENT, null, false);
  while (walker.nextNode()) {
    for (const attr of walker.currentNode.attributes) {
      if (attr.value.includes(fragment)) {
        attr.value = attr.value.replace(fragment, "#");
      }
    }
  }
  graph.setAttributeNS(xmlns, "xmlns", svgns);
  graph.setAttributeNS(xmlns, "xmlns:xlink", xlinkns);

  var cssStyleText = getCSSStyles( graph );
  appendCSS( cssStyleText, graph );

  const serializer = new window.XMLSerializer;
  const string = serializer.serializeToString(graph);
  var svgBlob = new Blob([string], {type: "image/svg+xml"});
  var svgUrl = URL.createObjectURL(svgBlob);
  var downloadLink = document.createElement("a");
  downloadLink.href = svgUrl;
  downloadLink.download = name;
  document.body.appendChild(downloadLink);
  downloadLink.click();
  document.body.removeChild(downloadLink);
}
/**
 * @name getCSSStyles
 * @param {SVGSVGElement} parentElement
 * @unpure {HTMLDocument} document
 * @returns {String}
 */
function getCSSStyles( parentElement ) {
  var selectorTextArr = [];

  // Add Parent element Id and Classes to the list
  selectorTextArr.push( '#'+parentElement.id );
  for (var c = 0; c < parentElement.classList.length; c++)
      if ( !contains('.'+parentElement.classList[c], selectorTextArr) )
        selectorTextArr.push( '.'+parentElement.classList[c] );

  // Add Children element Ids and Classes to the list
  var nodes = parentElement.getElementsByTagName("*");
  for (var i = 0; i < nodes.length; i++) {
    var id = nodes[i].id;
    if ( !contains('#'+id, selectorTextArr) )
      selectorTextArr.push( '#'+id );

    var classes = nodes[i].classList;
    for (var c = 0; c < classes.length; c++)
      if ( !contains('.'+classes[c], selectorTextArr) )
        selectorTextArr.push( '.'+classes[c] );
  }

  // Extract CSS Rules
  var extractedCSSText = "";
  for (var i = 0; i < document.styleSheets.length; i++) {
    var s = document.styleSheets[i];

    try {
        if(!s.cssRules) continue;
    } catch( e ) {
          if(e.name !== 'SecurityError') throw e; // for Firefox
          continue;
        }

    var cssRules = s.cssRules;
    for (var r = 0; r < cssRules.length; r++) {
      if ( contains( cssRules[r].selectorText, selectorTextArr ) )
        extractedCSSText += cssRules[r].cssText;
    }
  }

  return extractedCSSText;
}


////////////////////////////////////////////////////////////////////////////////
///    WORD CLOUD
////////////////////////////////////////////////////////////////////////////////

/**
 * @name drawWordCloud
 * @param {Array.<SVGCircleElement>} groups
 * @unpure {Object} d3
 * @unpure {Object} pubsub
 */
function drawWordCloud (groups) {
  let col   = {},
      arr   = [],
      count = 0;

  groups.forEach(function(g){
    let gid = (g.getAttribute("id")).replace("group","");
    let terms = d3.selectAll(".term").filter(".g-" + gid).nodes();
    terms.forEach(function(t){
      count ++;
      if (col[t.getAttribute("fdt")] == undefined) {
        col[t.getAttribute("fdt")] = {"freq" : 1, "label" : t.getAttribute("label")}
      } else {
        col[t.getAttribute("fdt")].freq = col[t.getAttribute("fdt")].freq + 1
      }
    })
  });

  arr = (Object.values(col)).map(function(l){
    return {"freq":(l.freq / count),"label":l.label};
  }).sort(function(l1,l2){
    return l2.freq - l1.freq;
  });

  if (arr.length === 0) {
    return;
  }

  var scaler = d3
    .scaleLinear()
    .domain([
      Math.log( (arr[ arr.length - 1 ]).freq ),
      Math.log( (arr[ 0 ]).freq )
    ])
    .range([
      0,
      1
    ]);

  arr.forEach(function(item, idx) {
    arr[idx].ratio = scaler( Math.log(item.freq) );
  });

  pubsub.publish(EXTRACTED_TERMS_EVENT, arr);
}

////////////////////////////////////////////////////////////////////////////////
///    ISO LINE
////////////////////////////////////////////////////////////////////////////////

/**
 * @name drawIsoLine
 * @param {Array} branches of <Gargantext.Components.PhyloExplorer.Types.Branch>
 * @unpure {Object} d3
 * @unpure {Object} label
 */
function drawIsoLine(branches) {
  var coordinates = getIsoLineCoordinates();

  var svg = d3
    .select(ISO_LINE_DOM_QUERY)
    .append("svg")
      .attr("width", coordinates.w)
      .attr("height", coordinates.h)
    .append("g");

  var centerColumnCoordinates = getCenterColumnCoordinates();

  // (?) Iso line width: full width of its div parent, will stretch the page
  //     Iso line real content: same length and x-position as the main scape SVG
  var xRange = coordinatesToXRange(centerColumnCoordinates);
  var yRange = coordinatesToYRange(coordinates);

  xScale0 = d3
    .scaleLinear()
    .domain([
      0,
      Math.max( ...branches.map(b => b.x1) )
    ])
    .range(xRange);

  yScale0 = d3
    .scaleLinear()
    .domain( d3.extent(branches, b => b.y) )
    .nice()
    .range(yRange);

  var density = d3
    .contourDensity()
    .x(function(b) {
      return xScale0(b.x1);
    })
    .y(function(b) {
      return yScale0(b.y);
    })
    .size([
      coordinates.w,
      coordinates.h
    ])
    .thresholds(
      Math.round(branches.length / 2)
    )
    (branches)

  /* shadows and lights */

  svg
    .append("g")
     .selectAll("circle")
     .data(branches)
     .enter()
     .append("circle")
       .attr("cx", b => xScale0(b.x1))
       .attr("cy", b => yScale0(b.y))
       .attr("r","55")
       .attr("id",b => "peak-shadow" + b.bId)
       .attr("visibility","visible")
       .style("fill","#FFFFFF");

  svg
    .selectAll("path")
    .data(density)
    .enter()
    .append("path")
      .attr("d", d3.geoPath())
      .attr("fill", "none")
      .attr("stroke", "#74B5FF")
      .attr("stroke-width", (d, i) => i % 2 ? 0.25 : 1)
      .attr("stroke-linejoin", "round");

  label =
    d3
      .select(ISO_LINE_DOM_QUERY)
      .append("div")
        .attr("class","peak-label");

  svg
    .append("g")
    .selectAll("text")
    .data(branches)
    .enter()
    .append("text")
      .attr("x", b => xScale0(b.x1))
      .attr("y", b => yScale0(b.y) + 4)
      .attr("class","peak")
      .attr("id",b => "peak-" + b.bId)
      .style("fill","#0d1824")
      .attr("visibility","visible")
      .text("▲")
    .on("mouseover", function(e, b) {
      peakOver(b, b.bId);
    })
    .on("mouseout", function(e, b) {
      peakOut(b, b.bId);
    })
    .on("click", function(e, b) {
      peakClick(b, coordinates);
    });
}
/**
 * @name getIsoLineCoordinates
 * @unpure {Object} d3
 * @returns {Object}
 *    <Float> x
 *    <Float> y
 *    <Float> w
 *    <Float> h
 *    <Float> t
 *    <Float> r
 *    <Float> b
 *    <Float> l
 */
function getIsoLineCoordinates() {
  var el = d3
  .select(ISO_LINE_DOM_QUERY)
  .node()
  .getBoundingClientRect();

  return {
    x: 0,
    y: 0,
    w: el.width,
    h: el.height,
    t: 16,
    r: 12,
    b: 16,
    l: 12
  };
}

////////////////////////////////////////////////////////////////////////////////
///    PHYLO
////////////////////////////////////////////////////////////////////////////////

/**
 * @name drawPhylo
 * @param {Array} branches of <Gargantext.Components.PhyloExplorer.Types.Branch>
 * @param {Array} periods of <Gargantext.Components.PhyloExplorer.Types.Period>
 * @param {Array} groups of <Gargantext.Components.PhyloExplorer.Types.Group>
 * @param {Array} links of <Gargantext.Components.PhyloExplorer.Types.Link>
 * @param {Array} aLinks of <Gargantext.Components.PhyloExplorer.Types.AncestorLink>
 * @param {Array} bLinks of <Gargantext.Components.PhyloExplorer.Types.BranchLink>
 * @param {Array<Number>} frame
 * @unpure {Window.<Boolean>} window.weighted
 * @unpure {Object} d3
 * @unpure {Object} svg instanceof d3.selection
 * @unpure {Object} panel instanceof d3.selection
 * @unpure {Function} zoom see https://github.com/d3/d3-zoom#zoom
 */
function drawPhylo(branches, periods, groups, links, aLinks, bLinks, frame) {

  drawIsoLine(branches);

  /* *** draw the phylo *** */

  var centerColumnCoordinates = getCenterColumnCoordinates();

  var scapeCoordinates = getScapeCoordinates();

  svg = d3
    .select('.phylo-grid__content__scape')
    .append("svg")
      .attr("width", scapeCoordinates.w)
      .attr("height", scapeCoordinates.h)

  /* labels */

  var firstDate = Math.min(...groups.map(g => (g.from).getFullYear()))
  var yLabels = (periods.map(p => ({y:p.y,from:p.from,to:p.to,label:(p.from).getFullYear()}))).filter(p => p.label >= firstDate);
  var xLabels = toXLabels(branches,groups,frame[2]);

  /* weight */

  if (window.weighted == true) {
    // var wInf = Math.min(...groups.map(g => g.weight))
    var wSup = Math.max(...groups.map(g => g.weight))
    var wScale = d3.scaleLog().domain([1,wSup]).range([3,10])

  }


  /* scales */
  var xRange = coordinatesToXRange(centerColumnCoordinates);
  var yRange = coordinatesToYRange(centerColumnCoordinates);

  var xScale = d3
    .scaleLinear()
    .domain([
      0,
      frame[2]
    ])
    .range(xRange);

  var yScale = d3
    .scaleTime()
    .domain( setYDomain(yLabels) )
    .range(yRange);

  /* mask */

  svg
    .append("defs")
    .append("svg:clipPath")
      .attr("id","mask")
    .append("svg:rect")
      .attr("width", centerColumnCoordinates.w)
      .attr("height", centerColumnCoordinates.h)
      .attr("x", centerColumnCoordinates.x)
      .attr("y", centerColumnCoordinates.y);

  /* panel */

  panel = svg
    .append("g")
    .attr("id", "panel")
    .attr("clip-path", "url(#mask)")

  /* highlight */

  xLabels.forEach(b =>
      panel.append("rect")
              .attr("class","branch-hover")
              .attr("x", xScale(b.inf))
              .attr("y", -10000)
              .attr("width", xScale(b.sup) - xScale(b.inf))
              .attr("height", 20000)
              .attr("id","hover-" + b.bId)
              .style("visibility","hidden"))

  yLabels.forEach(l =>
      panel.append("line")
           .attr("class","y-highlight")
           .attr("id","y-highlight-" + l.label)
           .attr("x1", -10000)
           .attr("y1", yScale(l.from))
           .attr("x2", 10000)
           .attr("y2", yScale(l.from))
           .style("visibility","hidden"))

  /* links */


  var linkGen = d3.linkVertical();
  var groupLinks = links.map(l => ({source: findGroup(groups, l.from, xScale, yScale), target: findGroup(groups, l.to, xScale, yScale),from: l.from, to: l.to, label: l.label}));

  var groupAncestors = aLinks.map(l => ({source: findGroup(groups, l.from, xScale, yScale), target: findGroup(groups, l.to, xScale, yScale),from: l.from, to: l.to, label: l.label}));

  panel
    .selectAll("path")
    .data(groupLinks.concat(groupAncestors))
    .join("path")
    .attr("d", linkGen)
    .attr("fill", "none")
    .attr("stroke","#0d1824")
    .attr("class", "group-path")
    .attr("source",d => d.from)
    .attr("target",d => d.to)
    .attr("label", d => d.label)
    // .on("click", function(){
    //   // console.log(this)
    // })

  // var colors = ["#F0684D","#aa8c58","#74b5ff","#0d1824"];

  /* groups */

  groups.forEach(g => setGroup(g, xScale, yScale, wScale));

  /* axis */

  var xAxis = svg
    .append("g")
      .attr("class","x-axis")
      .attr("transform", "translate(0," + centerColumnCoordinates.t + ")");

  var yAxis = svg
    .append("g")
      .attr("class","y-axis")
      .attr("transform", "translate(" + centerColumnCoordinates.x + ",0)");

  setAxisX(xScale,xLabels, branches, xAxis);
  setAxisY(yScale,yLabels, yAxis);

  /* zoom */

  // (!) Debouncing the whole "zoom" + "drag and drop" computation
  //
  //     Some browser engine are not yet optimized for managing this kind of
  //     heavy computations (especially with no treshold debounce, which can
  //     lead to performance worsen by a factor of 10)
  //     Google engine is also prone to change leading to various effects on
  //     heavy SVG or heavy SVG computation
  //
  //     Hence the act of debouncing. 20ms seems a sweet spot:
  //      - empirically does not treshold a lot of computation, minimising
  //        potential heavy job freezing ("doherty treshold" UX law)
  //      - imperceptible buffer job ("aesthetic-usability effect" UX law)
  var debouncedOnZoom = debounce(
    onZoom,
    20
  );

  zoom = d3
    .zoom()
    .scaleExtent([
      1,
      50
    ])
    .on("zoom", function(e) {
      debouncedOnZoom(
        e,
        centerColumnCoordinates,
        xScale,
        yScale,
        xLabels,
        yLabels,
        branches,
        xAxis,
        yAxis,
        xScale,
        yScale
      );
    });

  svg.call(zoom).on("dblclick.zoom",null).on("dblclick",doubleClick);


  /* role & dynamic */

  var emergences = getEmergences(groups, xScale, yScale);
  var branchByGroup = getBranchByGroup(groups);

  var keys = Object.keys(emergences);
  var freqs = (keys.map(k => window.freq[k])).filter(f => f != null);

  // var fontScale = d3.scaleLinear().domain([0,Math.max(...freqs)]).range([2,10]);
  var fontScale = d3.scaleLinear().domain([0,Math.sqrt(Math.max(...freqs))]).range([2,20]);
  var opacityScale = d3.scaleLinear().domain([0,1/Math.sqrt(Math.max(...freqs))]).range([0.1,1]);

  keys.forEach(function(k){
    addEmergenceLabels(
      k,
      emergences,
      branchByGroup,
      fontScale,
      opacityScale
    );
  });

  /* groups */

  d3.selectAll(".header").raise();
}
/**
 * @name getLeftColumnCoordinates
 * @unpure {Object} d3
 * @returns {Object}
 *    <Float> x
 *    <Float> y
 *    <Float> w
 *    <Float> h
 *    <Float> t
 *    <Float> r
 *    <Float> b
 *    <Float> l
 */
function getLeftColumnCoordinates() {
  var el = d3
    .select(LEFT_COLUMN_DOM_QUERY)
    .node()
    .getBoundingClientRect();

  return {
    x: 0,
    y: 0,
    w: el.width,
    h: el.height,
    t: 16,
    r: 0,
    b: 0,
    l: 0
  };
}
/**
 * @name getCenterColumnCoordinates
 * @unpure {Object} d3
 * @returns {Object}
 *    <Float> x
 *    <Float> y
 *    <Float> w
 *    <Float> h
 *    <Float> t
 *    <Float> r
 *    <Float> b
 *    <Float> l
 */
function getCenterColumnCoordinates() {
  var leftColumn     = getLeftColumnCoordinates();
  var elCenterColumn = d3
    .select(CENTER_COLUMN_DOM_QUERY)
    .node()
    .getBoundingClientRect();

  return {
    x: leftColumn.x + leftColumn.w,
    y: 32,
    w: elCenterColumn.width,
    h: elCenterColumn.height,
    t: 32,
    r: 8,
    b: 0,
    l: 8
  };
}
/**
 * @name getScapeColumnCoordinates
 * @unpure {Object} d3
 * @returns {Object}
 *    <Float> x
 *    <Float> y
 *    <Float> w
 *    <Float> h
 *    <Float> t
 *    <Float> r
 *    <Float> b
 *    <Float> l
 */
function getScapeCoordinates() {
  var el = d3
    .select(SCAPE_DOM_QUERY)
    .node()
    .getBoundingClientRect();

  return {
    x: 0,
    y: 0,
    w: el.width,
    h: el.height,
    t: 0,
    r: 0,
    b: 0,
    l: 0
  };
}
/**
 * @name toXLabels
 * @param {Array} branches of <Gargantext.Components.PhyloExplorer.Types.Branch>
 * @param {Array} groups of <Gargantext.Components.PhyloExplorer.Types.Group>
 * @param {Float} xMax
 * @returns {Object}
 *    <Int> x
 *    <String> label
 *    <Int> inf
 *    <Int> sup
 *    <Int> bId
 */
function toXLabels(branches, groups, xMax) {

  var xLabels = branches.map(function(b) {
      var bId = b.bId,
           xs = groups.filter(g => g.bId == bId).map(g => g.x),
          inf = Math.min(...xs),
          sup = Math.max(...xs);

      return { x : b.x2,
           label : b.label.replace(/\"/g, '').replace(/\|/g, ''),
             inf : inf,
             sup : sup,
             bId : bId};
  })

  return xLabels.map(function(b,i){
      var prec = 0,
          succ = xMax;

      if (i != 0)
          prec = xLabels[i -1].sup

      if (i != (xLabels.length - 1))
          succ = xLabels[i + 1].inf

      var w = Math.min(...[(b.x - prec) / 2,(succ - b.x) / 2]),
        inf = b.x - w,
        sup = b.x + w;

      inf = (b.inf < inf) ? b.inf : inf + (w / 10);
      sup = (b.sup > sup) ? b.sup : sup - (w / 10);

      return { x : (sup + inf) / 2,
           label : b.label,
             inf : inf,
             sup : sup,
             bId : b.bId};
  })
}
/**
 * @name xOverFlow
 * @param {Object} ticks instanceof d3.selection
 * @param {Array} arr <Array>
 *    <Float>
 *    <Float>
 * @unpure {Object} memoTickText
 *    <Int> => <TickText>
 */
function xOverFlow(ticks,arr) {
  var average = arr.reduce((a,b) => a + b[0], 0) / arr.length;
  var delta = 2;

  ticks.each(function(t,i){
    var text = d3.select(this),
         str = d3.select(this).text(),
       count = str.length,
          //  y = text.attr("y"),
          dy = parseFloat(text.attr("dy")),
         bId = arr[i][1],
       tspan = text
          .attr("bId", bId)
          .text(null)
          .append("tspan")
          .attr("x", 0)
          .attr("y", -14)
          .attr("dy", dy + "em");
          // .attr("bId","");

    var idx;
    var buffer = '';
    var node = tspan.node();
    var limit = arr[i][0] - delta;

    // Case a: Memoized pattern
    if (bId in memoTickText && memoTickText[ bId ].limit === limit) {
      return tspan.text( memoTickText[ bId ].text );
    }

    // Case b.1: Substractive pattern computation
    if (limit > average) {
      idx = count;

      while (idx > 2) {
        buffer = str.slice(0, idx)
        tspan.text( buffer );
        if (node.getComputedTextLength() < limit) {
          break;
        }
        idx = Math.floor(idx / 2)
      }
    }

    // Case b.2: Additive pattern computation
    if (limit <= average) {
      idx = 2;

      while (idx <= count) {
        buffer = str.slice(0, idx);
        tspan.text( buffer );
        if (node.getComputedTextLength() > limit) {
          break;
        }
        idx = Math.ceil(idx * 1.5);
      }
    }

    if (buffer.length !== count) {
      buffer = buffer.slice(0, -1)
             + '…';
      tspan.text( buffer );
    }

    // Store new value
    memoTickText[ bId ] = {
      bId: bId,
      limit: limit,
      text: buffer
    };
  });
}
/**
 * @name addMarkX
 * @param {Object} ticks instanceof d3.selection
 * @param {Array} ws <Float>
 * @param {Array} ids <Int>
 * @unpure {Object} d3
 * @unpure {Window.Array.<Int>} window.branchFocus
 */
function addMarkX(ticks,ws,ids) {
  ticks.each(function(t,i){
      d3.select(this)
        .append("rect")
        .attr("x","-" + (ws[i]/2 + 1))
        .attr("y","-4")
        .attr("height","8")
        .attr("width",ws[i] + 1)
        .attr("class","x-mark")
        .attr("id", "xmark-" + ids[i])

      if (window.branchFocus.includes(ids[i])) {
        d3.select("#xmark-" + ids[i]).style("fill","#F0684D");
      }
  })
}
/**
 * @name setMarkYLabel
 * @param {Object} labels instanceof d3.selection
 * @unpure {Object} d3
 */
function setMarkYLabel(labels) {
  labels.each(function(l,i){
      d3.select(this).attr("dx","-5").attr("class","y-label").attr("id","y-label-" +  d3.timeYear(l).getFullYear());
  })
}
/**
 * @name addMarkY
 * @param {Object} ticks instanceof d3.selection
 * @unpure {Object} d3
 */
function addMarkY(ticks) {
  ticks.each(function(d,i){
      if (d3.timeYear(d) < d) {
          // month
          d3.select(this)
            .append("circle").attr("cx",0).attr("cy",0).attr("r",3).attr("class","y-mark-month");
      } else {
          var from = d3.timeYear(d).getFullYear();
          // year
          d3.select(this)
            .append("circle").attr("cx",0).attr("cy",0).attr("r",6).attr("class","y-mark-year-outer").attr("id","y-mark-year-outer-" + from);
          d3.select(this)
            .append("circle").attr("cx",0).attr("cy",0).attr("r",3).attr("class","y-mark-year-inner").attr("id","y-mark-year-inner-" + from);
      }
  })
}
/**
 * @name setYDomain
 * @param {Object} labels
 *    <Date> from
 *    <String> label
 *    <Date> to
 *    <Int> y
 * @returns {Array}
 *    <Date>
 *    <Date>
 */
function setYDomain(labels) {
  var ts = ["week","month","day","year","epoch"];

  //console.log(labels)

  if (ts.includes(window.timeScale)) {
    labels = labels.sort(function(d1,d2){return d1.from - d2.from;})
  }

  var inf = (labels[0]).from,
      sup = (labels[labels.length - 1]).to;

  if (window.timeScale == "week") {
    inf =  addDays(inf,7)
    sup = addDays(sup,7)
  } else if (window.timeScale == "month") {
    inf = removeDays(inf,31)
    sup = addDays(sup,31)
  } else if (window.timeScale == "day") {
    inf = removeDays(inf,1)
    sup = addDays(sup,1)
  } else if (window.timeScale == "year") {
    inf = removeDays(inf,365)
    sup = addDays(sup,365)
  } else if (window.timeScale == "epoch") {
    inf = inf
    sup = sup
  } else {
    inf = new Date((inf.getFullYear() - 1),0,0);
    sup = new Date((sup.getFullYear() + 1),0,0);
  }

  // inf = new Date((inf - 1),6,0);
  // inf = new Date((1950 - 1),6,0);
  // sup = new Date((sup + 1),0,0);

  return [inf,sup];
}
/**
 * @name findGroup
 * @param {Array} groups of <Gargantext.Components.PhyloExplorer.Types.Group>
 * @param {Int} id
 * @param {Function<Int>} xsc see https://github.com/d3/d3-scale#_continuous
 * @param {Function<Int>} ysc see https://github.com/d3/d3-scale#_continuous
 * @returns {Array}
 *    <Float>
 *    <Float>
 */
function findGroup (groups, id, xsc, ysc) {
  var group = groups.find(g => g.gId == id);
  var x = xsc(group.x);
  var y = ysc(group.to);

  return [x,y]
}
/**
 * @name coordinatesToBox
 * @param {Object} coordinates
 *    <Float> x
 *    <Float> y
 *    <Float> w
 *    <Float> h
 * @returns {Array<Float>}
 */
function coordinatesToBox(coordinates) {
  return [
    coordinates.x,
    coordinates.y,
    coordinates.w,
    coordinates.h
  ];
}
/**
 * @name coordinatesToXRange
 * @param {Object} coordinates
 *    <Float> x
 *    <Float> w
 *    <Float> r
 *    <Float> l
 * @returns {Array<Float>}
 */
function coordinatesToXRange(coordinates) {
  var lower = coordinates.x
            + coordinates.r
            + coordinates.l;

  var upper = coordinates.x
            - coordinates.r
            - coordinates.l
            + coordinates.w;

  return [lower, upper];
}
/**
 * @name coordinatesToYRange
 * @param {Object} coordinates
 *    <Float> y
 *    <Float> h
 *    <Float> t
 *    <Float> b
 * @returns {Array<Float>}
 */
function coordinatesToYRange(coordinates) {
  var lower = coordinates.y
            + coordinates.t
            + coordinates.b;

  var upper = coordinates.y
            - coordinates.t
            - coordinates.b
            + coordinates.h;

  return [lower, upper];
}
/**
 * @name textWidth
 * @param {String} text
 * @returns {CanvasRenderingContext2D}
 */
function textWidth(text) {
  const context = document.createElement("canvas").getContext("2d");
  return context.measureText(text).width;
}
/**
 * @name toLines
 * @param {String} words
 * @param {Array<Int>} fdt
 * @param {Array<Int>} role
 * @param {Float} targetWidth
 * @returns {Array<Object>}
 *    <Float> width
 *    <Array<String>> text
 *    <Array<Int>> fdt
 *    <Array<Int>> role
 */
function toLines(words,fdt,role,targetWidth) {
  let line;
  let lineWidth0 = Infinity;
  const lines = [];
  for (let i = 0, n = words.length; i < n; ++i) {
    let lineText1 = (line ? line.text + " " : "") + words[i];
    // let lineFdt1 = (line ? line.fdt + " " : "") + fdt[i];
    // let lineRole1 = (line ? line.role + " " : "") + role[i];
    let lineWidth1 = textWidth(lineText1);
    if ((lineWidth0 + lineWidth1) / 2 < targetWidth + 10) {
      line.width = lineWidth0 = lineWidth1;
      // line.text = lineText1;
      line.text.push(words[i])
      line.fdt.push(fdt[i])
      line.role.push(role[i])
    } else {
      lineWidth0 = textWidth(words[i]);
      line = {width: lineWidth0, text: [words[i]], fdt: [fdt[i]], role: [role[i]]};
      lines.push(line);
    }
  }
  return lines;
}
/**
 * @name toTextRadius
 * @param {Array<Object>}
 *    <Float> width
 *    <Array<String>> text
 *    <Array<Int>> fdt
 *    <Array<Int>> role
 * @param {Int} lineHeight
 * @returns {Float}
 */
function toTextRadius(lines,lineHeight) {
  let radius = 0;
  for (let i = 0, n = lines.length; i < n; ++i) {
    const dy = (Math.abs(i - n / 2 + 0.5) + 2) * lineHeight;
    const dx = lines[i].width / 2;
    const sdy = Math.pow(dy, 2);
    const sdx = Math.pow(dx, 2);
    radius = Math.max(radius, Math.sqrt(sdx + sdy));
  }
  return radius;
}
/**
 * @name findFreq
 * @param {Int} fdt
 * @unpure {Window.Array<Int>} window.freq
 * @returns {Int}
 */
function findFreq(fdt) {
  let freq = 0;
  if (window.freq[fdt] != null) {
    freq = window.freq[fdt]
  }
  return freq;
}
/**
 * @name findRole
 * @param {Int} r
 * @returns {String}
 */
function findRole(r) {
  if (r == 0) {
    return " emerging";
  } else if (r == 2) {
    return " decreasing";
  } else {
    return "";
  }
}
/**
 * @name mergeLists
 * @param {Array<String>} l1
 * @param {Array<Int>} l2
 * @param {Array<Int>} l3
 * @returns {Array<Array>}
 *    <String>
 *    <Int>
 *    <Int>
 */
function mergeLists(l1,l2,l3) {
  let merged = [];
  for (let i = 0; i < l1.length; i++) {
        merged.push([l1[i],l2[i],l3[i]])
  }
  return merged;
}
/**
 * @name setAxisX
 * @param {Function<Int>} scale see https://github.com/d3/d3-scale#_continuous
 * @param {Array<Object>} labels
 *    <Float> x
 *    <String> label
 *    <Float> inf
 *    <Float> sup
 *    <Int> bId
 * @param {Array} branches of <Gargantext.Components.PhyloExplorer.Types.Branch>
 * @param {Object} xAxis instanceof d3.selection
 * @unpure {Object} d3
 */
function setAxisX(scale, labels, branches, xAxis) {
  xAxis.call(d3.axisTop(scale)
                  .tickValues(labels.map(l => l.x))
                  .tickFormat((l, i) => labels[i].label)
                  .tickSizeOuter(0));
  xAxis.selectAll(".tick text")
       .call(xOverFlow, labels.map(l => [scale(l.sup) - scale(l.inf),l.bId]))
       .on("mouseover", function() {
          tickOver(this, branches);
        })
       .on("click", function() {
         tickClick(branches, this);
       })
       .on("mouseout" , function() {
          tickOut(this, branches);
        });
  xAxis.selectAll(".tick line").remove();
  xAxis.selectAll(".tick rect").remove();
  xAxis.selectAll(".tick")
       .call(addMarkX, labels.map(l => scale(l.sup) - scale(l.inf)),labels.map(l => l.bId));
}
/**
 * @name setAxisY
 * @param {Function<Int>} scale see https://github.com/d3/d3-scale#_continuous
 * @param {Array<Object>} labels
 *    <Date> from
 *    <Int> label
 *    <Date> to
 *    <Float> y
 * @param {Object} yAxis instanceof d3.selection
 * @unpure {Object} d3
 */
function setAxisY(scale,labels, yAxis) {
  yAxis.call(d3.axisLeft(scale)
                  .tickFormat(function(d){
                      if (d3.timeYear(d) < d) {
                          // '%B'
                          return d3.timeFormat('%d %B')(d);
                      } else {
                          return d3.timeFormat('%Y')(d);
                      }
                  })
                  .tickSizeOuter(0));
  yAxis.selectAll(".tick line").remove();
  yAxis.selectAll(".tick circle").remove();
  yAxis.selectAll(".tick")
       .call(addMarkY)
  yAxis.selectAll(".tick text")
       .call(setMarkYLabel)
}
/**
 * @name setGroupClass
 * @param {Object} g <Gargantext.Components.PhyloExplorer.Types.Group>
 * @returns {String}
 */
function setGroupClass(g) {
  var str = "group-inner" + " " + "branch-" + g.bId;
  for (var i = 0; i < g.source.length; i++) {
    str += " source-" + g.source[i];
  }
  return str;
}
/**
 * @name setGroup
 * @param {Object} g <Gargantext.Components.PhyloExplorer.Types.Group>
 * @param {Function<Int>} xScale see https://github.com/d3/d3-scale#_continuous
 * @param {Function<Int>} yScale see https://github.com/d3/d3-scale#_continuous
 * @param {Function<Int>} wScale see https://github.com/d3/d3-scale#_continuous
 * @unpure {Window.<Boolean>} window.weighted
 * @unpure {Object} d3
 */
function setGroup(g, xScale, yScale, wScale) {

  // console.log(window.weighted)

  if(window.weighted == true) {
    var radius = wScale(g.weight)
  } else {
    var radius = 5;
  }

  // var radius = 5;

  // var col = Math.round(Math.random() * 3) - 1

  panel
    .append("circle")
    .attr("class","group-outer")
    .attr("cx", xScale(g.x))
    .attr("cy", yScale(g.to))
    .attr("r" , radius + 0.5);

  panel
    .append("circle")
    .attr("class", setGroupClass(g))
    .attr("cx", xScale(g.x))
    .attr("cy", yScale(g.to))
    .attr("bId", g.bId)
    .attr("id"  ,  "group" + g.gId)
    .attr("gid"  , g.gId)
    .attr("r" ,radius)
    // .attr("stroke",colors[col])
    .attr("stroke","#0d1824")
    .style("fill", "#61a3a9")
    .attr("from",(g.to).getFullYear())
    // .on("mouseover",groupOver)
    // .on("mouseout" ,groupOut)

  /* group label */

  var lineHeight = 12,
      targetWidth = Math.sqrt(textWidth(g.label.join('').trim()) * radius),
      lines = toLines(g.label,g.foundation,g.role,targetWidth),
      textRadius = toTextRadius(lines,lineHeight),
      textRatio = (radius - 0.5) / textRadius;

  for (let i = 0; i < lines.length; i++) {

    let words  = lines[i].text,
        fdt    = lines[i].fdt,
        roles  = lines[i].role,
        terms  = mergeLists(words,fdt,roles),
        toSpan = (acc, w) => acc + "<tspan fdt=" + w[1]
                                 + " class='term fdt-" + w[1] + " " + "g-" + g.gId + findRole(w[2]) + "'"
                                 + " gy=" + yScale(g.to)
                                 + " gx=" + xScale(g.x)
                                 + " freq=" + findFreq(w[1])
                                 + " label='" + w[0] + "'"
                                 + " gid=" + g.gId
                                 + " bid=" + g.bId
                                 + " from="   + (g.to).getFullYear()
                                 + ">" + w[0] + "</tspan>";

    panel
        .append("text")
        .attr("class","ngrams")
        .attr("text-anchor", "middle")
        .style("font-size", 12 * textRatio + "px")
        .style("visibility", "hidden")
          .append("tspan")
            .attr("x", xScale(g.x))
            .attr("y", yScale(g.to) + (i - lines.length / 2.8) * (lineHeight * textRatio))
            .html(terms.reduce(toSpan,""));


    d3.selectAll(".term")
      .on("click",function(){
        termClick(this.textContent,this.getAttribute("fdt"),this.getAttribute("gid"),"group");
      })
      // .on("mouseover",function(){
      //   d3.selectAll(".term").classed("term-unfocus",true);
      //   d3.selectAll(".term").filter(".g-" + this.getAttribute("gid")).classed("term-focus",true);
      // })
      // .on("mouseout",function(){
      //   d3.selectAll(".term").classed("term-unfocus",false);
      //   d3.selectAll(".term").classed("term-focus",false);
      // });

  }
}
/**
 * @name getBranchByGroup
 * @param {Array} groups of <Gargantext.Components.PhyloExplorer.Types.Group>
 * @returns {Array<Array<Int>>}
 */
function getBranchByGroup(groups) {
  var branchByGroup = {};

  groups.forEach(function(g) {
    // is a term in many branches ?
    for (var i = 0; i < (g.foundation).length; i++) {
      var fdt = (g.foundation)[i];
      if (fdt in branchByGroup) {
        (branchByGroup[fdt]).push(g.bId);
      } else {
        branchByGroup[fdt] = [g.bId];
      }
    }
  });

  return branchByGroup;
}
/**
 * @name getEmergences
 * @param {Array} groups of <Gargantext.Components.PhyloExplorer.Types.Group>
 * @param {Function<Int>} xScale see https://github.com/d3/d3-scale#_continuous
 * @param {Function<Int>} yScale see https://github.com/d3/d3-scale#_continuous
 * @returns {Object}
 *      <Int> => <Object>
 *          <Int> bId
 *          <String> label
 *          <Array<Float>> x
 *          <Array<Float>> y
 */
function getEmergences(groups, xScale, yScale) {
  var emergences = {};

  groups.forEach(function(g) {
    // is emerging ?
    if ((g.role).includes(0)) {
      for (var i = 0; i < (g.role).length; i++) {
        if ((g.role)[i] == 0) {
          var gf = (g.foundation)[i];
          if (gf in emergences) {
            (emergences[gf].x).push(xScale(g.x));
            (emergences[gf].y).push(yScale(g.to));
          } else {
            emergences[gf] = {"label":g.label[i],"x":[xScale(g.x)],"y":[yScale(g.to)],"bid":g.bId}
          }
        }
      }
    }
  });

  return emergences;
}
/**
 * @name addEmergenceLabels
 * @param {String} k
 * @param {Object} emergences
 *      <Int> => <Object>
 *          <Int> bId
 *          <String> label
 *          <Array<Float>> x
 *          <Array<Float>> y
 * @param {Array<Array<Int>>} branchByGroup
 * @param {Function<Int>} fontScale see https://github.com/d3/d3-scale#_continuous
 * @param {Function<Int>} opacityScale see https://github.com/d3/d3-scale#_continuous
 * @unpure {Window.<Array<Int>>} window.freq
 * @unpure {Object} panel instanceof d3.selection
 * @unpure {Object} pubsub
 */
function addEmergenceLabels(k, emergences, branchByGroup, fontScale, opacityScale){
  let x = ((emergences[k]).x).reduce(arraySum) / ((emergences[k]).x).length;
  let y = ((emergences[k]).y).reduce(arraySum) / ((emergences[k]).y).length;
  var freq = 0;

  if (k in window.freq) {
    freq = window.freq[k];
  }

  var xr = x + (rdm() * Math.random() * 10);
  var yr = y + (rdm() * Math.random() * 10);

  // add header label text
  panel
    .append("text")
    .attr("x", xr)
    .attr("y", yr)
    .attr("fdt",k)
    .attr("id","head" + k)
    .attr("mem-size", fontScale(Math.sqrt(freq)))
    .attr("mem-opac", opacityScale(Math.sqrt(freq)))
    .attr("bid",(emergences[k]).bid)
    .style("font-size", fontScale(Math.sqrt(freq)) + "px")
    .style("opacity", opacityScale(1/Math.sqrt(freq)))
    .attr("class","header")
    .style("text-anchor", "middle")
    // .style("fill",(bid.length > 1) ? "#012840" : "#CC382F")
    .text((emergences[k]).label)
    .on("mouseover", function() {
      var wrapper = d3.select("#wrapper-" + this.id);
      // show header wrapper
      wrapper.classed("header-wrapper--hover", true);
      // little tweak to put elements into the foreground (~zIndex top)
      panel.node().appendChild( wrapper.node() );
      panel.node().appendChild( this );
    })
    .on("mouseout", function() {
      // hide header wrapper
      d3
        .select("#wrapper-" + this.id)
        .classed("header-wrapper--hover", false);
    })
    .on("click",function(){
      showLabel();
      termClick((emergences[k]).label,k,k,"head");
      pubsub.publish(DISPLAY_VIEW_EVENT, "labelMode");
      // remove wrapper header (preventing little issue where wrapper is still
      // displayed because "mouseout" event has been disrupted by "click" event)
      d3
        .select("#wrapper-" + this.id)
        .classed("header-wrapper--hover", false);
    });


  // add header wrapper surrounding text
  // (based on its text width and height)
  var bbox = d3
    .select("#head" + k)
    .node()
    .getBoundingClientRect();

  // (?) Empirical proportional padding value (due to text size diffences)
  var padding = {
    t: 0,
    r: bbox.width * 0.05,
    b: bbox.height * 0.4,
    l: bbox.width * 0.05
  };

  var w3
    =
    + bbox.width
    + padding.l
    + padding.r;

  var h3
    =
    + bbox.height
    + padding.t
    + padding.b;

  var x3
    =
    + xr
    - (bbox.width / 2)
    - padding.l;

  var y3
    =
    + yr
    - bbox.height
    - padding.t;

  panel
    .append("rect", "text")
    .attr("x", x3)
    .attr("y", y3)
    .attr("width", w3)
    .attr("height", h3)
    .attr("class", "header-wrapper")
    .attr("id", "wrapper-head" + k)
}

////////////////////////////////////////////////////////////////////////////////
///    EXPORTS
////////////////////////////////////////////////////////////////////////////////

exports._extractedTermsEvent    = EXTRACTED_TERMS_EVENT;
exports._extractedCountEvent    = EXTRACTED_COUNT_EVENT;
exports._selectedTermEvent      = SELECTED_TERM_EVENT;
exports._selectedBranchEvent    = SELECTED_BRANCH_EVENT;
exports._selectedSourceEvent    = SELECTED_SOURCE_EVENT;
exports._displayViewEvent       = DISPLAY_VIEW_EVENT;

exports._drawPhylo        = drawPhylo;
exports._drawWordCloud    = drawWordCloud;
exports._showLabel        = showLabel;
exports._termClick        = termClick;
exports._resetView        = resetView;
exports._showLabel        = showLabel;
exports._showHeading      = showHeading;
exports._showLanding      = showLanding;
exports._exportViz        = exportViz;
exports._doubleClick      = doubleClick;
exports._highlightGroups  = highlightGroups;
exports._initPath         = initPath;

exports._publish          = pubsub.publish;
exports._subscribe        = pubsub.subscribe;
exports._unsubscribe      = pubsub.unsubscribe;
