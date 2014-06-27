var $builtinmodule = function (name) {
    var module = {};

    module.summarize = new Sk.builtin.func(function (x, y) {
        return Sk.builtin.asnum$(x + y);
    });

    return module;
};
