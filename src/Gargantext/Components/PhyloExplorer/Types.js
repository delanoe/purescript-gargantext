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
