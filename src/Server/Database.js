var types = require('pg').types;

exports.setUpConversions = function () {
      types.setTypeParser(types.builtins.INT8, function(val) {
            return parseInt(val);
      });
      // types.setTypeParser(types.builtins.TIMESTAMPTZ, fromEpoch);
      // types.setTypeParser(types.builtins.TIMESTAMP, fromEpoch);
}

// function fromEpoch(raw) {
//       return new Date(raw + 'Z').getTime();
// }