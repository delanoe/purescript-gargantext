exports._drawPhylo = drawPhylo;
exports._drawWordCloud = drawWordCloud;

// set javascript date from a string year
function yearToDate(year) {
  var d = new Date()
  d.setYear(parseInt(year));
  d.setMonth(0);
  d.setDate(1);
  return d;
}

function stringToDate(str) {
var arr = (str.replace('"','')).split('-');
var d = new Date();
d.setYear(parseInt(arr[0]));
d.setMonth(parseInt(arr[1]));
d.setMonth(d.getMonth() - 1);
d.setDate(parseInt(arr[2]));
return d;
}

function utcStringToDate(str) {
var arr = ((str.replace('"','')).replace(' UTC','')).split(/[\s-:]+/);
var d = new Date();
d.setYear(parseInt(arr[0]));
d.setMonth(parseInt(arr[1]));
d.setDate(parseInt(arr[2]));
d.setHours(parseInt(arr[3]),parseInt(arr[4]),parseInt(arr[5]))
return d;
}

function stringArrToArr(str) {
var arr = ((str.replace('["','')).replace('"]','')).split('","');
return arr;
}

function intArrToArr(int) {
var arr = ((int.replace('[','')).replace(']','')).split(',');
return arr;
}

function yearToDateHacked(w) {
var d = new Date(2020,0,0);
d.setDate(d.getDate() + (w * 7));
return d
}

function toCoord(id) {
  var x = parseFloat(d3.select("#group" + id).attr("cx")),
      y = parseFloat(d3.select("#group" + id).attr("cy"));
  return [x,y];
}

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

function xOverFlow(ticks,arr) {
  ticks.each(function(t,i){
      var text = d3.select(this),
         chars = d3.select(this).text().split('').reverse(),
            nb = chars.length,
             y = text.attr("y"),
            dy = parseFloat(text.attr("dy")),
          line = [],
         tspan = text.attr("bId",arr[i][1]).text(null).append("tspan").attr("x", 0).attr("y", -14).attr("dy", dy + "em").attr("bId","");

      while ((char = chars.pop()) && (tspan.node().getComputedTextLength() < (arr[i][0] - 2))) {
          line.push(char);
          tspan.text(line.join(''));
      }

      if (line.length != nb) {
          line.slice(-3)
          tspan.text(line.join('') + '...')
      }

  })
}

var branchFocus = [];

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

      if (branchFocus.includes("" + ids[i])) {
        d3.select("#xmark-" + ids[i]).style("fill","#F0684D");
      }
  })
}

function setMarkYLabel(labels) {
  labels.each(function(l,i){
      d3.select(this).attr("dx","-5").attr("class","y-label").attr("id","y-label-" +  d3.timeYear(l).getFullYear());
  })
}

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

function addDays(date, days) {
  var result = new Date(date);
  result.setDate(result.getDate() + days);
  return result;
}

function removeDays(date, days) {
  var result = new Date(date);
  result.setDate(result.getDate() - days);
  return result;
}

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
};


function findValueByPrefix(prefix) {
  for(var i = 0; i < window.terms.length; i++) {
    var object = (window.terms)[i];
      if(object.label.toLowerCase().startsWith(prefix.toLowerCase()))
      {
          return object;
      }

  }
  return null;
}

 function drawWordCloud (groups) {

    let labels = {},
        count  = 0;

    d3.selectAll(".word-cloud").remove();

    groups.forEach(function(g){
      let gid = (g.getAttribute("id")).replace("group","");
      let terms = d3.selectAll(".term").filter(".g-" + gid).nodes();
      terms.forEach(function(t){
        count ++;
        if (labels[t.getAttribute("fdt")] == undefined) {
          labels[t.getAttribute("fdt")] = {"freq" : 1, "label" : t.getAttribute("label")}
        } else {
          labels[t.getAttribute("fdt")].freq = labels[t.getAttribute("fdt")].freq + 1
        }
      })
    });

    labels = (Object.values(labels)).map(function(l){
      return {"freq":(l.freq / count),"label":l.label};
    }).sort(function(l1,l2){
      return l2.freq - l1.freq;
    })

    let y = 20
    let opacity = d3.scaleLinear().domain([Math.log((labels[labels.length - 1]).freq),Math.log((labels[0]).freq)]).range([0.5,1]);

    labels.forEach(function(l){
      y = y + 12;
      window.svg3.append("text")
          .attr("class","word-cloud")
          .attr("x", 10)
          .attr("y", y)
          .style("opacity", opacity(Math.log(l.freq)))
          .text(l.label);
    })
  }


function drawPhylo(branches, periods, groups, links, aLinks, bLinks, frame) {

  /* ** draw the sources box ** */

  document.querySelector("#checkSource").style.display  = "inline-block";

  /* ** draw the search box ** */

  var inputSearch = document.getElementById("search-box");
  inputSearch.style.visibility = "visible";
  inputSearch.addEventListener("keyup", autocomplete);
  document.getElementById("search-autocomplete").style.visibility = "visible";
  document.getElementById("search-label").style.visibility = "visible";

  /* ** draw the isoline ** */

  d3.select('#phyloIsoLine').style("background","#EBE4DD");

  var div0 = getIsolineDOMElement(),
        m0 = {t:5,r:5,b:5,l:5},
        w0 = div0.width,
        h0 = div0.height;

  var svg0 = d3
        .select('#phyloIsoLine')
        .append("svg")
          .attr("width", w0)
          .attr("height",h0)
        .append("g");

  var xScale0 = d3.scaleLinear().domain([0,Math.max(...branches.map(b => b.x1))]).range([2 * m0.l, w0 - 2 * m0.l]),
      yScale0 = d3.scaleLinear().domain(d3.extent(branches, b => b.y)).nice().range([2 * m0.t, h0 - 2 * m0.t]);

  var density =
      d3.contourDensity()
          .x(function(b) { return xScale0(b.x1); })
          .y(function(b) { return yScale0(b.y); })
          .size([w0, h0])
          .thresholds(Math.round(branches.length / 2))
          (branches)

  function getIsolineDOMElement() {
    return d3.select('#phyloIsoLine').node().getBoundingClientRect();
  }

  /* shadows and lights */

  svg0.append("g")
     .selectAll("circle")
     .data(branches)
     .enter()
     .append("circle")
       .attr("cx", b => xScale0(b.x1))
       .attr("cy", b => yScale0(b.y))
       .attr("r","55")
       .attr("id",b => "peak-shadow" + b.bId)
       .attr("visibility","visible")
       .style("fill","#f5eee6");

  svg0.selectAll("path")
     .data(density)
     .enter()
     .append("path")
       .attr("d", d3.geoPath())
       .attr("fill", "none")
       .attr("stroke", "#74B5FF")
       .attr("stroke-width", (d, i) => i % 2 ? 0.25 : 1)
       .attr("stroke-linejoin", "round");


  var label =
      d3.select("#phyloIsoLine")
        .append("div")
        .attr("class","peak-label");

  svg0.append("g")
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
         peakClick(b, b.bId);
       });

  /* *** draw the phylo *** */

  var div1 = d3.select('#phyloScape')
               .node().getBoundingClientRect(),
      div2 = d3.select('#phyloTimeline')
               .node().getBoundingClientRect(),
         m = {t:10, b:10, l:10, r:10},
         w = div1.width,
         h = div1.height
      ydiv = div1.y;

  const svg = d3
      .select('#phyloScape')
      .append("svg")
      .attr("viewBox",[0,0,w,h]);

  var xo = div2.width + m.l,
      yo = 2.5 * m.t,
      wo = w - xo - m.r,
      ho = h - yo - m.b;

  /* *** draw the graph *** */

  var div3 = d3.select('#phyloGraph')
               .node().getBoundingClientRect();

  window.svg3 = d3
        .select('#phyloGraph')
        .append("svg")
          .attr("width", div3.width)
          .attr("height",div3.height)
        .append("g");


  /* labels */

  var firstDate = Math.min(...groups.map(g => (g.from).getFullYear()))

  var yLabels = (periods.map(p => ({y:p.y,from:p.from,to:p.to,label:(p.from).getFullYear()}))).filter(p => p.label >= firstDate);
  var xLabels = toXLabels(branches,groups,frame[2]);

  /* weight */

  if (window.weighted == true) {
    var wInf = Math.min(...groups.map(g => g.weight))
    var wSup = Math.max(...groups.map(g => g.weight))
    var wScale = d3.scaleLog().domain([1,wSup]).range([3,10])

  }


  /* scales */

  var xScale = d3.scaleLinear().domain([0,frame[2]]).range([xo + m.t,wo + xo]),
      yScale = d3.scaleTime().domain(setYDomain(yLabels)).range([m.t + yo, ho + yo]);

  /* panel and& mask */

  var mask = svg
      .append("defs")
      .append("svg:clipPath")
          .attr("id","mask")
      .append("svg:rect")
          .attr("width", wo)
          .attr("height",ho)
          .attr("x",xo)
          .attr("y",yo);

  const panel = svg.append("g").attr("clip-path", "url(#mask)").attr("id","panel")

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

  function findGroup (id, xsc, ysc) {
      var group = groups.find(g => g.gId == id),
          x = xsc(group.x);
          y = ysc(group.to);
      return [x,y]
  }

  var linkGen = d3.linkVertical();
  var groupLinks = links.map(l => ({source: findGroup(l.from, xScale, yScale), target: findGroup(l.to, xScale, yScale),from: l.from, to: l.to, label: l.label}));

  var groupAncestors = aLinks.map(l => ({source: findGroup(l.from, xScale, yScale), target: findGroup(l.to, xScale, yScale),from: l.from, to: l.to, label: l.label}));

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

  var colors = ["#F0684D","#aa8c58","#74b5ff","#0d1824"];

  /* groups */

  groups.forEach(g => setGroup(g));

  /* axis */

  var xAxis = svg.append("g").attr("class","x-axis").attr("transform", "translate(0," + yo + ")"),
      yAxis = svg.append("g").attr("class","y-axis").attr("transform", "translate(" + xo + ",0)");

  setAxisX(xScale,xLabels);
  setAxisY(yScale,yLabels);

  /* zoom */

  var zoom = d3.zoom()
      .scaleExtent([1,50])
      .extent([[xo,yo],[wo,ho]])
      .on("zoom", function(e) {
        debouncedOnZoom(e);
      });

  svg.call(zoom).on("dblclick.zoom",null).on("dblclick",doubleClick);

  d3.select("#reset").on("click",reset);

  function reset() {
    svg.transition()
        .duration(750)
        .call(zoom.transform, d3.zoomIdentity);
  }

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
    };
  };

  var debouncedOnZoom = debounce(
    onZoom
    , 50
  );

  function onZoom(event) {
      var zoomX = event.transform.rescaleX(xScale),
          zoomY = event.transform.rescaleY(yScale),
          zoomXLabels = xLabels
              .filter(b => (zoomX(b.x) >= xo) && (zoomX(b.x) <= (wo + xo))),
          zoomYLabels = yLabels
              .filter(p => (zoomY(p.y) >= yo) && (zoomY(p.y) <= (ho + yo)));

      setAxisX(zoomX,zoomXLabels);

      setAxisY(zoomY,zoomYLabels);

      panel.selectAll("circle").attr("transform", event.transform);
      panel.selectAll("text").attr("transform", event.transform);
      panel.selectAll("path").attr("transform", event.transform);
      panel.selectAll(".branch-hover").attr("transform", event.transform);
      panel.selectAll(".y-highlight").attr("transform", event.transform);
      panel.selectAll(".ngrams").attr("transform", event.transform);
      panel.selectAll(".term-path").attr("transform", event.transform);
      panel.selectAll(".emergence").attr("transform", event.transform);
      panel.selectAll(".header").attr("transform", event.transform);

      showPeak()
  }

  /* label */

  // https://observablehq.com/@d3/parallel-coordinates

  d3.select("#label").on("click", function(e, l) {
    showLabel(l);
  });

  function autocomplete(e) {
      var txt = e.target.value;

      if (txt.length < 1) {
        document.getElementById("search-autocomplete").value = '';
        return;
      }

      var placeholder = findValueByPrefix(txt);

      if (placeholder !== null) {
        document.getElementById("search-autocomplete").value = placeholder.label;
      } else {
        document.getElementById("search-autocomplete").value = '';
      }

      // press enter
      if (e.keyCode === 13) {
        e.preventDefault();
        if (placeholder !== null) {
          showLabel("search")
          termClick(placeholder.label,placeholder.fdt,0,"search")
        }
      }
  }

  function showLabel(type) {
    if ((document.getElementsByClassName("header"))[0].style.visibility != "hidden") {
      showHeading()
    }
    doubleClick()
    let ngrams = document.getElementsByClassName("ngrams")
    let groups = document.getElementsByClassName("group-inner")
    if (ngrams[0].style.visibility == "hidden") {
      document.getElementById("label").classList.add("labeled")
      for (var i = 0; i < groups.length; i++) {
        groups[i].style.fill = "#fff";
      }
      for (var i = 0; i < ngrams.length; i++){
        ngrams[i].style.visibility = "visible";
      }
    } else {
      if (type != "search") {
        document.getElementById("label").classList.remove("labeled")
        for (var i = 0; i < groups.length; i++) {
          groups[i].style.fill = "#61a3a9";
        }
        for (var i = 0; i < ngrams.length; i++){
          ngrams[i].style.visibility = "hidden";
        }
      }
    }
  }

  /* role & dynamic */

  d3.select("#heading").on("click",showHeading);

  var emergences = {};
  var branchByGroup = {};
  groups.forEach(function(g){
    // is a term in many branches ?
    for (var i = 0; i < (g.foundation).length; i++) {
      var fdt = (g.foundation)[i];
      if (fdt in branchByGroup) {
        (branchByGroup[fdt]).push(g.bId);
      } else {
        branchByGroup[fdt] = [g.bId];
      }
    }
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

  var keys = Object.keys(emergences);
  var freqs = (keys.map(k => window.freq[k])).filter(f => f != null);

  const arraySum = (acc, curr) => acc + curr;

  function rdm() {
    if (Math.random() > 0.5) {
      return 1;
    } else {
      return -1;
    }
  }

  // var fontScale = d3.scaleLinear().domain([0,Math.max(...freqs)]).range([2,10]);
  var fontScale = d3.scaleLinear().domain([0,Math.sqrt(Math.max(...freqs))]).range([2,20]);
  var opacityScale = d3.scaleLinear().domain([0,1/Math.sqrt(Math.max(...freqs))]).range([0.1,1]);

  keys.forEach(function(k){
    let x = ((emergences[k]).x).reduce(arraySum) / ((emergences[k]).x).length;
    let y = ((emergences[k]).y).reduce(arraySum) / ((emergences[k]).y).length;
    let bid = Array.from(new Set(branchByGroup[k]));
    var freq = 0;
    // console.log(k)

    if (k in window.freq) {
      freq = window.freq[k];
    }
    panel.append("text")
         .attr("x",x + (rdm() * Math.random() * 10))
         .attr("y",y + (rdm() * Math.random() * 10))
         .attr("fdt",k)
         .attr("id","head" + k)
         .attr("mem-size", fontScale(Math.sqrt(freq)))
         .attr("mem-opac", opacityScale(Math.sqrt(freq)))
         .attr("bid",(emergences[k]).bid)
         .style("font-size", fontScale(Math.sqrt(freq)) + "px")
         .style("opacity", opacityScale(1/Math.sqrt(freq)))
         .attr("class","header")
         .style("visibility","hidden")
         .style("text-anchor", "middle")
         // .style("fill",(bid.length > 1) ? "#012840" : "#CC382F")
         .style("fill",(bid.length > 1) ? "#012840" : "#012840")
         .text((emergences[k]).label)
         .on("click",function(){
            showLabel("header")
            termClick((emergences[k]).label,k,k,"head");
         });
  });

  function landingView() {
    window.ldView = true;
    doubleClick()
    let headers = document.getElementsByClassName("header")
    let groups = document.getElementsByClassName("group-inner")
    if (headers[0].style.visibility == "hidden") {
      document.getElementById("heading").classList.add("headed")
      for (var i = 0; i < groups.length; i++) {
        groups[i].style.fill = "#f5eee6";
        groups[i].classList.add("group-heading")
      }
      d3.selectAll(".group-path").classed("path-heading",true);
      for (var i = 0; i < headers.length; i++){
        headers[i].style.visibility = "visible";
      }
    } else {
      document.getElementById("heading").classList.remove("headed")
      for (var i = 0; i < groups.length; i++) {
        groups[i].style.fill = "#61a3a9";
        groups[i].classList.remove("group-heading")
      }
      d3.selectAll(".group-path").classed("path-heading",false);
      for (var i = 0; i < headers.length; i++){
        headers[i].style.visibility = "hidden";
      }
    }
  }

  landingView()

  function showHeading() {
    if ((document.getElementsByClassName("ngrams"))[0].style.visibility != "hidden") {
      showLabel("header")
    }
    landingView()
  }

  /* groups */

  function textWidth(text) {
      const context = document.createElement("canvas").getContext("2d");
      return context.measureText(text).width;
  }

  function toLines(words,fdt,role,targetWidth) {
    let line;
    let lineWidth0 = Infinity;
    const lines = [];
    for (let i = 0, n = words.length; i < n; ++i) {
      let lineText1 = (line ? line.text + " " : "") + words[i];
      let lineFdt1 = (line ? line.fdt + " " : "") + fdt[i];
      let lineRole1 = (line ? line.role + " " : "") + role[i];
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

  function findFreq(fdt) {
    let freq = 0;
    if (window.freq[fdt] != null) {
      freq = window.freq[fdt]
    }
    return freq;
  }

  function setGroupClass(g) {
    var str = "group-inner" + " " + "branch-" + g.bId;
    for (var i = 0; i < g.source.length; i++) {
      str += " source-" + g.source[i];
    }
    return str;
  }

  function setGroup(g) {

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
          textRadius = toTextRadius(lines,lineHeight)
          textRatio = (radius - 0.5) / textRadius;

      for (let i = 0; i < lines.length; i++) {

        let words  = lines[i].text,
            fdt    = lines[i].fdt,
            roles  = lines[i].role,
            terms  = mergeLists(words,fdt,roles)
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

  d3.selectAll(".header").raise();


  function findRole(r) {
    if (r == 0) {
      return " emerging";
    } else if (r == 2) {
      return " decreasing";
    } else {
      return "";
    }
  }

  function mergeLists(l1,l2,l3) {
    let merged = [];
    for (let i = 0; i < l1.length; i++) {
          merged.push([l1[i],l2[i],l3[i]])
    }
    return merged;
  }

  function setAxisY(scale,labels) {
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

  function setAxisX(scale,labels) {
      xAxis.call(d3.axisTop(scale)
                      .tickValues(labels.map(l => l.x))
                      .tickFormat((l, i) => labels[i].label)
                      .tickSizeOuter(0));
      xAxis.selectAll(".tick text")
           .call(xOverFlow, labels.map(l => [scale(l.sup) - scale(l.inf),l.bId]))
           .on("mouseover", tickOver)
           .on("click", tickClick)
           .on("mouseout" , tickOut);
      xAxis.selectAll(".tick line").remove();
      xAxis.selectAll(".tick rect").remove();
      xAxis.selectAll(".tick")
           .call(addMarkX, labels.map(l => scale(l.sup) - scale(l.inf)),labels.map(l => l.bId));
  }

  function showPeak() {
      d3.selectAll(".peak").style("fill",function(peak,i){
          var isVisible = d3
              .selectAll(".branch-" + i)
              .nodes()
              .map(function(g){
                  var x = g.getBoundingClientRect().x,
                      y = g.getBoundingClientRect().y;
                  if ((x >= xo) && (x  <= (wo + xo)) && (y >= (div1.y + yo - m.t)) && (y <= (div1.y + ho + yo))) {
                      return true;
                  } else {
                      return false;
                  }})
              .reduce((mem,cur) => {return mem || cur;})
          if (isVisible) {
              d3.select("#peak-shadow" + i).attr("visibility","visible");
              return "#0d1824";
          } else {
              d3.select("#peak-shadow" + i).attr("visibility","hidden");
              return "#A9A9A9";
          }
      })
  }

  function countTerms(groups) {
    var terms = [];
    for (var i = 0; i < groups.length; i++) {
      let gid = ((groups[i].getAttribute("id")).split("group"))[1]
      d3.selectAll(".g-" + gid).nodes().forEach(e => terms.push(e.getAttribute("fdt")))
    }
    return (Array.from(new Set(terms))).length;
  }

  function countBranches(groups) {
    var branches = [];
    for (var i = 0; i < groups.length; i++) {
      branches.push(groups[i].getAttribute("bId"));
    }
    return (Array.from(new Set(branches))).length;
  }

  function highlightGroups (groups) {

    window.ldView = false;

    // console.log(groups)

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

    document.querySelector("#phyloGroups").innerHTML = groups.length;
    document.querySelector("#phyloTerms").innerHTML = countTerms(groups);
    document.querySelector("#phyloBranches").innerHTML = countBranches(groups);
    document.querySelector("#phyloGroups").classList.add("phylo-focus");
    document.querySelector("#phyloTerms").classList.add("phylo-focus");
    document.querySelector("#phyloBranches").classList.add("phylo-focus");

    // highlight the links

    for (var i = 0; i < paths.length; i++) {
      if (gids.includes((paths[i]).getAttribute("source")) && (paths[i]).getAttribute("target")) {
        paths[i].classList.add("path-focus");
        paths[i].classList.remove("path-unfocus");
      }
    }

  }

  function termClick (txt,idx,nodeId,typeNode) {

    // remove old focus
    initPath()

    // catch the last transformations
    if (typeNode == "group") {
      var transform = d3.select("#group" + nodeId).node().getAttribute("transform");
    } else if (typeNode == "head") {
      var transform = d3.select("#head" + nodeId).node().getAttribute("transform");
    } else {
      var transform = (d3.selectAll(".header").nodes())[0].getAttribute("transform");
    }

    // focus

    document.querySelector("#phyloPhylo").innerHTML = txt;
    document.querySelector("#phyloPhylo").classList.add("phylo-focus");
    document.querySelector("#phyloSearch").setAttribute("href",'https://en.wikipedia.org/w/index.php?search="' + txt + '"')

    // highlight the groups

    var terms = document.getElementsByClassName("fdt-" + idx),
        periods = groupTermsBy(terms,"from");

    var groups  = [];

    for (var i = 0; i < terms.length; i++) {
      groups.push(d3.select("#group" + (terms[i]).getAttribute("gid")));
      branchFocus.push((terms[i]).getAttribute("bid"));
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

  function peakOver (b,i) {
      var el = getIsolineDOMElement();
      d3.select("#peak-" + i).classed("peak-focus",false);
      d3.select("#peak-" + i).classed("peak-over",true);
      label.text(b.label.replace(/"/g,''))
           .style("visibility", "visible")
           .style("top", (yScale0(b.y) + el.top  - 18) + "px")
           .style("left",(xScale0(b.x1) + el.left + 12) + "px");
      branchOver(b.bId);
  }

  function peakOut (b,i) {
      d3.select("#peak-" + i).classed("peak-over",false);
      if (branchFocus.includes("" + b.bId)) {
        d3.select("#peak-" + i).classed("peak-focus",true);
      }
      branchOut();
  }

  function peakClick (b,i) {
    initPath()
    let groups = d3.selectAll(".group-inner").filter(".branch-" + b.bId).nodes()
    branchFocus.push(b.bId);
    /* word cloud */

    drawWordCloud(groups);
    highlightGroups(groups);
    /* rescale */
    let tx = (groups[0]).getAttribute("cx")
    svg.transition()
        .duration(750)
        .call(zoom.transform, d3.zoomIdentity.translate(((wo + xo) / 2) - tx, 0).scale(1));

    d3.selectAll(".path-unfocus").lower();
  }



  function branchOver(bId) {
      // headers
      if (d3.select("#heading").classed("headed")) {
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

  function headerOut() {
    d3.selectAll(".header").nodes().forEach(function(header){
      header.style["font-size"] = header.getAttribute("mem-size") + "px";
      header.style["opacity"] = header.getAttribute("mem-opac");
    })
  }

  function branchOut(bId) {
      d3.selectAll(".peak-label").style("visibility","hidden");
      d3.selectAll(".branch-hover").style("visibility","hidden");
      d3.selectAll(".x-mark").style("fill","#4A5C70");
      for (var i = 0; i < branchFocus.length; i++) {
        d3.select("#xmark-" + branchFocus[i]).style("fill","#F24C3D");
      }
      headerOut();
  }

  function tickClick() {
    initPath()
    let bid = this.getAttribute("bId"),
        groups = d3.selectAll(".group-inner").filter(".branch-" + bid).nodes();

    // draw the word cloud

    branchFocus.push(bid);
    drawWordCloud(groups);

    // highlight the groups

    highlightGroups(groups);
    d3.selectAll(".path-unfocus").lower();
  }

  function tickOver() {
      var ego = this.getAttribute("bId"),
          branch = branches.find(b => b.bId == ego);
      if (d3.select("#peak-" + ego).node().style.visibility != "hidden") {
          branchOver(ego);
          peakOver(branch,ego);
      }
  }

  function tickOut() {
      var ego = this.getAttribute("bId"),
          branch = branches.find(b => b.bId == ego);
      branchOut();
      peakOut(branch,ego)
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

  function initPath () {
    window.highlighted = true;
    window.ldView = false;
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
    branchFocus = [];
  }

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
    document.querySelector("#phyloPhylo").innerHTML = "phylomemy";
    document.querySelector("#phyloPhylo").classList.remove("phylo-focus");
    document.querySelector("#phyloGroups").innerHTML = window.nbGroups;
    document.querySelector("#phyloTerms").innerHTML = window.nbTerms;
    document.querySelector("#phyloBranches").innerHTML = window.nbBranches;
    document.querySelector("#phyloGroups").classList.remove("phylo-focus");
    document.querySelector("#phyloTerms").classList.remove("phylo-focus");
    document.querySelector("#phyloBranches").classList.remove("phylo-focus");
    d3.selectAll(".peak").classed("peak-focus",false);
    d3.selectAll(".peak").classed("peak-focus-source",false);
    d3.selectAll(".x-mark").style("fill","#4A5C70");
    branchFocus = [];
  }

  /* export */

  d3.select("#export").on("click",exportViz);

  function exportViz() {


const xmlns = "http://www.w3.org/2000/xmlns/";
const xlinkns = "http://www.w3.org/1999/xlink";
const svgns = "http://www.w3.org/2000/svg";

var time = new Date();

serialize(svg.node(),"phylomemy-" + Date.parse(time.toString())  + ".svg")
function serialize(graph,name) {
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
};


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

  function contains(str,arr) {
    return arr.indexOf( str ) === -1 ? false : true;
  }

}

function appendCSS( cssText, element ) {
  var styleElement = document.createElement("style");
  styleElement.setAttribute("type","text/css");
  styleElement.innerHTML = cssText;
  var refNode = element.hasChildNodes() ? element.children[0] : null;
  element.insertBefore( styleElement, refNode );
}
}

}
