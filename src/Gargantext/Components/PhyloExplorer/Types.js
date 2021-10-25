'use strict';
/**
 * @name yearToDate
 * @param {string} year
 * @returns {Date}
 */
function yearToDate(year) {
  var d = new Date();

  d.setYear(parseInt(year));
  d.setMonth(0);
  d.setDate(1);

  return d;
}
/**
 * @name stringToDate
 * @param {string} str
 * @returns {Date}
 */
function stringToDate(str) {
  var arr = (str.replace('"','')).split('-');
  var d = new Date();

  d.setYear(parseInt(arr[0]));
  d.setMonth(parseInt(arr[1]));
  d.setMonth(d.getMonth() - 1);
  d.setDate(parseInt(arr[2]));

  return d;
}
/**
 * @name utcStringToDate
 * @param {string} str
 * @returns {Date}
 */
function utcStringToDate(str) {
  var arr = ((str.replace('"','')).replace(' UTC','')).split(/[\s-:]+/);
  var d = new Date();

  d.setYear(parseInt(arr[0]));
  d.setMonth(parseInt(arr[1]));
  d.setDate(parseInt(arr[2]));
  d.setHours(parseInt(arr[3]), parseInt(arr[4]), parseInt(arr[5]))

  return d;
}

exports.yearToDate      = yearToDate;
exports.stringToDate    = stringToDate;
exports.utcStringToDate = utcStringToDate;

function draw(json) {

  // groups

  window.weighted = false;

  var groups = json.objects.filter(node => node.nodeType == "group").map(function(g){

    // console.log(g.weight)

    if ((g.weight != undefined) && (g.weight != "Nothing"))
      window.weighted = true;

    var keys = (g.foundation.slice(1, g.foundation.length - 1)).split('|')

    var labels =  (g.lbl.slice(1, g.lbl.length - 1)).split('|')

    for (var i = 0; i < keys.length; i++) {
      // freq
      if (!((keys[i]).trim() in window.freq)) {
        window.freq[(keys[i]).trim()] =  0;
      } else {
        window.freq[(keys[i]).trim()] += 1;
      }
      // terms
      if (!((keys[i]).trim() in window.terms)) {
        window.terms[(keys[i]).trim()] = {label:(labels[i]).trim(),fdt:(keys[i]).trim()}
      }
    }

    var from = yearToDate(g.from),
          to = yearToDate(g.to),
          weight = 0;
          source = [];

    if (g.strFrom != undefined) {
      if (window.timeScale == "epoch") {
        from = utcStringToDate(g.strFrom)
      } else {
        from = stringToDate(g.strFrom)
      }
    }

    if (g.strTo != undefined) {
      if (window.timeScale == "epoch") {
        to = utcStringToDate(g.strTo)
      } else {
        to = stringToDate(g.strTo)
      }
    }

    if (g.source != undefined)
      source =  intArrToArr(g.source);

    if (g.weight != undefined)
      weight = parseFloat((g.weight).replace("Just ",""));

    return { from : from,
                to : to,
                x : parseFloat(((g.pos).split(','))[0])  ,
                y : parseFloat(((g.pos).split(','))[1])  ,
              bId : parseInt(g.bId)    ,
              gId : parseInt(g._gvid)  ,
              size : parseInt(g.support),
            source : source,
            weight : weight,
            label : labels,
        foundation : keys,
              role : ((g.role.slice(1, g.role.length - 1)).split('|')).map(e => parseInt(e.trim()))}
  });

  var links = json.edges.filter(edges => edges.edgeType == "link").map(function(l){
    return {  lId : parseInt(l._gvid),
              from : parseInt(l.tail) ,
                to : parseInt(l.head) ,
            label : l.label}
  });

  var aLinks = json.edges.filter(edges => edges.edgeType == "ancestorLink").map(function(l){
    return {  lId : parseInt(l._gvid),
              from : parseInt(l.tail) ,
                to : parseInt(l.head) ,
            label : l.label }
  });

  var bLinks = json.edges.filter(edges => edges.edgeType == "branchLink").map(function(l){
    return { from : parseInt(l.tail) ,
                to : parseInt(l.head) }
  });

  window.terms = Object.values(window.terms)

  // draw the phylo

  drawPhylo(branches,periods,groups,links,aLinks,bLinks,bb);
}
