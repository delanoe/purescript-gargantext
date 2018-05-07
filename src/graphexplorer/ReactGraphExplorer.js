var React = require('react');
var PropTypes = require('prop-types');

var graphExplorer = require('graph-explorer');

console.log(graphExplorer);

const GraphExplorer = graphExplorer.default;

class ReactGraphExplorer extends React.Component {
  constructor(props) {
    super(props);

    this.ge = new GraphExplorer(props.settings, props.handlers);
    this.state = { graph: props.graph, mode: props.mode };

    this.initRenderer = this.initRenderer.bind(this);
  }

  componentWillReceiveProps(nextProps) {
    console.log(this.ge);
    if (nextProps.graph) {
      this.setState({ graph: nextProps.graph }, () => {
        this.ge.loadGraph(this.state.graph);
        this.ge.clusterize();
        this.ge.spatialize();
      });
    }

    this.setState({ mode: nextProps.mode }, () => {
      this.ge.sigma.settings('mode', this.state.mode);
    });
  }

  initRenderer(container) {
    this.ge.addRenderer(container);
  }

  render() {
    return React.createElement('div', { id: 'ge-container', ref: this.initRenderer }, null);
  }
}

ReactGraphExplorer.propTypes = {
  graph: PropTypes.shape({
    nodes: PropTypes.arrayOf(
      PropTypes.shape({
        id: PropTypes.string.isRequired,
        label: PropTypes.string,
        x: PropTypes.number,
        y: PropTypes.number,
        size: PropTypes.number,
        type: PropTypes.string,
        attributes: PropTypes.object // TODO(lucas): Specify this further.
      })
    ).isRequired,
    edges: PropTypes.arrayOf(
      PropTypes.shape({
        id: PropTypes.string.isRequired,
        source: PropTypes.string.isRequired,
        target: PropTypes.string.isRequired,
        weight: PropTypes.number // NOTE(lucas): required?
      }).isRequired
    )
  }),
  settings: PropTypes.shape({
    // TODO(lucas)
  }),
  mode: PropTypes.string,
  handlers: PropTypes.shape({
    overNode: PropTypes.func,
    outNode: PropTypes.func,
    clickNode: PropTypes.func,
    doubleClickNode: PropTypes.func,
    rightClickNode: PropTypes.func,
    overEdge: PropTypes.func,
    outEdge: PropTypes.func,
    clickEdge: PropTypes.func,
    doubleClickEdge: PropTypes.func,
    rightClickEdge: PropTypes.func,
    clickStage: PropTypes.func,
    doubleClickStage: PropTypes.func,
    rightClickStage: PropTypes.func
  })
};

export default ReactGraphExplorer;
