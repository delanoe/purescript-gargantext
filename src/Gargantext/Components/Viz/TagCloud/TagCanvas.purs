

{-
-- Imported from a Django Template I made
-- Example, see: https://alexandre.delanoe.org/affiliations

tagCloud :: [String] -> TagCloud String
tagCloud = undefined 

tagCloudCpt :: TagCloud String -> R.Element
tagCloudCpt = undefined


<script src="{% static "js/tagcanvas.js" %}" type="text/javascript"></script>
    <script type="text/javascript">
      window.onload = function() {
        try {
          TagCanvas.Start('myCanvas','tags',{
            textColour: '#030329',
            outlineColour: '#030329',
            reverse: true,
            depth: 0.8,
            maxSpeed: 0.05
          });
        } catch(e) {
          // something went wrong, hide the canvas container
          document.getElementById('myCanvasContainer').style.display = 'none';
        }
      };
</script>

{% block content %}

<div class="container theme-showcase" role="main">
        <div class="jumbotron">
						<div class="row">

		<div class="col-lg-3">
        <h1>Affiliations</h1>
		</div>
		<div class="col-lg-3"></div>
		<div class="col-lg-3"></div>
		<div class="col-lg-5"></div>
 
    <div id="myCanvasContainer">
      <canvas width="170" height="170" id="myCanvas">
        <p>Anything in here will be replaced on browsers that support the canvas element</p>
      </canvas>
    </div>
    </div>
    
		
		{% if tags_list %}
		<div id="tags">
			<ul>
			{% for tag in tags_list %}
			<li><a href="">{{tag}}</a></li>
			<li><a href="">{{tag}}</a></li>
			<li><a href="">{{tag}}</a></li>
			{% endfor %}
			</ul>
		</div>
    {% endif %}

</div>





{% for a in author.affiliation_set.all %} 
            <div class="col-lg-5">

                <h2> {{ a.institution.name }} </h2>
                Laboratory: {{ a.institution.laboratory }} 
                
                {% if a.institution.department %}
                . {{ a.institution.department }}
                {% endif %}

                <br>
                Role: {{ a.relation.title }} <br>

                {% if a.institution.website %}
                <p align="center">
                <a class="btn btn-xs btn-default" href={{ a.institution.website }} role="button">Website</a>
                </p>
                {% endif %}

</div>

{% endfor %}

</div>
{% endblock %}


