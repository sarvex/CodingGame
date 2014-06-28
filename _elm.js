var $builtinmodule = function (name) {
    var module = {};

    module._pick_material = new Sk.builtin.func(function (x, y) {
        return ELM_API._pick_material(Sk.ffi.remapToJs(x), Sk.ffi.remapToJs(y));
    });

    return module;
};
