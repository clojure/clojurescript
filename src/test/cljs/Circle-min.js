var React = require('./react-min');

var Circle = React.createClass({
  render: function() {
    return(
      <svg width="200px" height="200px" className="center">
        <circle cx="100px" cy="100px" r="100px" fill={this.props.color}>
        </circle>
      </svg>
    );
  }
});

module.exports = Circle;
