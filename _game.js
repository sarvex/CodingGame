var $builtinmodule = function (name) {
    var module = {};

    module.summarize = new Sk.builtin.func(function (x, y) {
        var a = ELM_API.summarize(x,  y);
        return Sk.builtin.asnum$(a);
    });

    return module;
};
