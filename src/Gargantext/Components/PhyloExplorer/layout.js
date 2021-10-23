'use strict';

function readJson(file, callback) {
  var raw = new XMLHttpRequest();
  raw.overrideMimeType("application/json");
  raw.open("GET", file, true);
  raw.onreadystatechange = function() {
    if (raw.readyState === 4 && raw.status == "200") {
      callback(raw.responseText);
    }
  }
  raw.send(null);
}

function unhide(mode) {
  document.querySelector("#reset").style.visibility     = "visible";
  document.querySelector("#label").style.visibility     = "visible";
  document.querySelector("#heading").style.visibility   = "visible";
  if (mode != "static") {
    document.querySelector("#export").style.visibility  = "visible";
  }
}

window.addEventListener("load", function() {
  // read the config
  readJson("./config.json",function(data1){
    var conf = JSON.parse(data1);
    // available config modes are "static" or "explorable"
    if (conf.mode == "static") {
      var path = "";
      var name = "";
      if (conf.path == null || conf.path == "") {
        path = conf.defaultPath;
        name = conf.defaultName;
      } else {
        path = conf.path;
        name = conf.pathName;
      }
      document.querySelector("#file-label").style.display   = "none";
      document.querySelector("#spin").style.visibility      = "visible";
      document.getElementById("phyloName").innerHTML        = name;
      document.querySelector("#phyloName").style.visibility = "visible";
      readJson(path,function(data2){
        phylo = JSON.parse(data2);
        unhide("static");
        draw(phylo);
      })
    }
  })
})

function readPhylo(file) {
  var reader = new FileReader();
  reader.onload = (function(f) {
    return function(e) {
      try {
        json = JSON.parse(e.target.result);
        unhide("explorable");
        draw(json)
      } catch (error) {
        console.log(error)
      }
    };
  })(file);
  reader.readAsText(file, "UTF-8");
}

// display the Draw button after loading a phylo
document.querySelector("#file-path").onchange = function(){
    document.querySelector("#file-name").textContent =  (this.files[0].name).substring(0,15) + "...";
    // document.querySelector("#file-name").textContent =  this.files[0].name;
    document.querySelector("#draw").style.display    = "inline-block";
}

// draw the phylo
document.querySelector("#draw").onclick = function() {
  document.querySelector("#spin").style.visibility  = "visible";
  readPhylo(document.getElementById("file-path").files[0]);
}


function drawPhyloInfo(elemClass, docs, foundations, branches, groups, terms, periods) {

  ReactDOM.render(React.createElement(phyloCorpus,{}),document.getElementById('phyloCorpus'));

  ReactDOM.render(React.createElement(phyloPhylo,{}),document.getElementById('phyloPhylo'));

  ReactDOM.render(React.createElement(phyloHow,{}),document.getElementById('phyloHow'));

  ReactDOM.render(React.createElement(phyloCorpusInfo,{
    nbDocs : docs,
    nbFoundations : foundations,
    nbPeriods : periods
  }),document.getElementById('phyloCorpusInfo'));

  ReactDOM.render(React.createElement(phyloPhyloInfo,{
    nbTerms : terms,
    nbGroups : groups,
    nbBranches : branches
  }),document.getElementById('phyloPhyloInfo'));

}

function draw(json) {

  // draw PhyloInfo

  window.freq  = {};
  window.terms = {};
  window.sources = [];

  window.nbDocs     = parseFloat(json.phyloDocs);
  window.nbBranches = parseFloat(json.phyloBranches);
  window.nbGroups   = parseFloat(json.phyloGroups);
  window.nbTerms    = parseFloat(json.phyloTerms);
  window.nbPeriods  = parseFloat(json.phyloPeriods);
  window.nbFoundations = parseFloat(json.phyloFoundations);
  window.timeScale = json.phyloTimeScale;

  if (json.phyloSources != undefined) {
    var sources = stringArrToArr(json.phyloSources);
    var checkSource = document.getElementById("checkSource");

    for (var i = 0; i < sources.length; i++) {
      window.sources.push({source:sources[i],index:i});
    }

    window.sources.sort(function(a, b){
      if(a.source < b.source) { return -1; }
      if(a.source > b.source) { return  1; }
      return 0;
    })

    for (var i = 0; i < window.sources.length; i++) {
      var option = document.createElement("option");
      option.text  = window.sources[i].source;
      option.value = window.sources[i].index;
      checkSource.add(option);
    }

  }

  // original bounding box

  bb = ((json.bb).split(',')).map(xy => parseFloat(xy))

  drawPhyloInfo("", window.nbDocs, window.nbFoundations, window.nbBranches, window.nbGroups, window.nbTerms, window.nbPeriods)

  // draw PhyloIsoline

  var branches = json.objects.filter(node => node.nodeType == "branch").map(function(b){
    return { x1 : b.branch_x ,
              y : b.branch_y ,
              x2 : parseFloat(((b.pos).split(','))[0]) ,
          label : b.label,
            bId : parseInt(b.bId),
            gvid : parseInt(b._gvid)
            }
  });

  var periods = json.objects.filter(node => node.nodeType == "period").map(function(p){

    var from = yearToDate(p.from),
          to = yearToDate(p.to);

    if (p.strFrom != undefined) {
      if (window.timeScale == "epoch") {
        from = utcStringToDate(p.strFrom)
      } else {
        from = stringToDate(p.strFrom)
      }
    }

    if (p.strTo != undefined) {
      if (window.timeScale == "epoch") {
        to = utcStringToDate(p.strTo)
      } else {
        to = stringToDate(p.strTo)
      }
    }

    return { from : from,
                to : to,
                y : parseFloat(((p.pos).split(','))[1])}
  });

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
