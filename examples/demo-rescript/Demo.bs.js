(() => {
  var __defProp = Object.defineProperty;
  var __export = (target2, all) => {
    for (var name2 in all)
      __defProp(target2, name2, { get: all[name2], enumerable: true });
  };

  // node_modules/rescript/lib/es6/caml_array.js
  function sub(x, offset, len) {
    var result = new Array(len);
    var j = 0;
    var i4 = offset;
    while (j < len) {
      result[j] = x[i4];
      j = j + 1 | 0;
      i4 = i4 + 1 | 0;
    }
    ;
    return result;
  }
  function set(xs, index, newval) {
    if (index < 0 || index >= xs.length) {
      throw {
        RE_EXN_ID: "Invalid_argument",
        _1: "index out of bounds",
        Error: new Error()
      };
    }
    xs[index] = newval;
  }
  function get(xs, index) {
    if (index < 0 || index >= xs.length) {
      throw {
        RE_EXN_ID: "Invalid_argument",
        _1: "index out of bounds",
        Error: new Error()
      };
    }
    return xs[index];
  }
  function make(len, init2) {
    var b2 = new Array(len);
    for (var i4 = 0; i4 < len; ++i4) {
      b2[i4] = init2;
    }
    return b2;
  }
  function blit(a1, i1, a2, i22, len) {
    if (i22 <= i1) {
      for (var j = 0; j < len; ++j) {
        a2[j + i22 | 0] = a1[j + i1 | 0];
      }
      return;
    }
    for (var j$1 = len - 1 | 0; j$1 >= 0; --j$1) {
      a2[j$1 + i22 | 0] = a1[j$1 + i1 | 0];
    }
  }

  // node_modules/rescript/lib/es6/curry.js
  function app(_f, _args) {
    while (true) {
      var args = _args;
      var f2 = _f;
      var init_arity = f2.length;
      var arity = init_arity === 0 ? 1 : init_arity;
      var len = args.length;
      var d = arity - len | 0;
      if (d === 0) {
        return f2.apply(null, args);
      }
      if (d >= 0) {
        return function(f3, args2) {
          return function(x) {
            return app(f3, args2.concat([x]));
          };
        }(f2, args);
      }
      _args = sub(args, arity, -d | 0);
      _f = f2.apply(null, sub(args, 0, arity));
      continue;
    }
    ;
  }
  function _1(o2, a0) {
    var arity = o2.length;
    if (arity === 1) {
      return o2(a0);
    } else {
      switch (arity) {
        case 1:
          return o2(a0);
        case 2:
          return function(param2) {
            return o2(a0, param2);
          };
        case 3:
          return function(param2, param$1) {
            return o2(a0, param2, param$1);
          };
        case 4:
          return function(param2, param$1, param$2) {
            return o2(a0, param2, param$1, param$2);
          };
        case 5:
          return function(param2, param$1, param$2, param$3) {
            return o2(a0, param2, param$1, param$2, param$3);
          };
        case 6:
          return function(param2, param$1, param$2, param$3, param$4) {
            return o2(a0, param2, param$1, param$2, param$3, param$4);
          };
        case 7:
          return function(param2, param$1, param$2, param$3, param$4, param$5) {
            return o2(a0, param2, param$1, param$2, param$3, param$4, param$5);
          };
        default:
          return app(o2, [a0]);
      }
    }
  }
  function _2(o2, a0, a1) {
    var arity = o2.length;
    if (arity === 2) {
      return o2(a0, a1);
    } else {
      switch (arity) {
        case 1:
          return app(o2(a0), [a1]);
        case 2:
          return o2(a0, a1);
        case 3:
          return function(param2) {
            return o2(a0, a1, param2);
          };
        case 4:
          return function(param2, param$1) {
            return o2(a0, a1, param2, param$1);
          };
        case 5:
          return function(param2, param$1, param$2) {
            return o2(a0, a1, param2, param$1, param$2);
          };
        case 6:
          return function(param2, param$1, param$2, param$3) {
            return o2(a0, a1, param2, param$1, param$2, param$3);
          };
        case 7:
          return function(param2, param$1, param$2, param$3, param$4) {
            return o2(a0, a1, param2, param$1, param$2, param$3, param$4);
          };
        default:
          return app(o2, [
            a0,
            a1
          ]);
      }
    }
  }
  function _3(o2, a0, a1, a2) {
    var arity = o2.length;
    if (arity === 3) {
      return o2(a0, a1, a2);
    } else {
      switch (arity) {
        case 1:
          return app(o2(a0), [
            a1,
            a2
          ]);
        case 2:
          return app(o2(a0, a1), [a2]);
        case 3:
          return o2(a0, a1, a2);
        case 4:
          return function(param2) {
            return o2(a0, a1, a2, param2);
          };
        case 5:
          return function(param2, param$1) {
            return o2(a0, a1, a2, param2, param$1);
          };
        case 6:
          return function(param2, param$1, param$2) {
            return o2(a0, a1, a2, param2, param$1, param$2);
          };
        case 7:
          return function(param2, param$1, param$2, param$3) {
            return o2(a0, a1, a2, param2, param$1, param$2, param$3);
          };
        default:
          return app(o2, [
            a0,
            a1,
            a2
          ]);
      }
    }
  }

  // lib/es6/src/html/Html.bs.js
  var Html_bs_exports = {};
  __export(Html_bs_exports, {
    $$Attr: () => $$Attr,
    $$Node: () => $$Node,
    $$int: () => $$int$1,
    $$var: () => $$var,
    a: () => a,
    abbr: () => abbr,
    accept: () => accept,
    accesskey: () => accesskey,
    action: () => action,
    address: () => address,
    area: () => area,
    article: () => article,
    aside: () => aside,
    attr: () => attr,
    audio: () => audio,
    autocomplete: () => autocomplete,
    autofocus: () => autofocus,
    b: () => b,
    base: () => base,
    bdi: () => bdi,
    bdo: () => bdo,
    blockquote: () => blockquote,
    br: () => br,
    button: () => button,
    canvas: () => canvas,
    caption: () => caption,
    charset: () => charset,
    checked: () => checked2,
    cite: () => cite,
    class_flags: () => class_flags,
    class_list: () => class_list,
    class_name: () => class_name,
    code: () => code2,
    col: () => col,
    colgroup: () => colgroup,
    cols: () => cols,
    command: () => command,
    content: () => content,
    contenteditable: () => contenteditable,
    datalist: () => datalist,
    dd: () => dd,
    defer: () => defer,
    del: () => del,
    details: () => details,
    dfn: () => dfn,
    dir: () => dir,
    disabled: () => disabled,
    div: () => div2,
    dl: () => dl,
    draggable: () => draggable,
    dt: () => dt,
    elem: () => elem$1,
    em: () => em,
    embed: () => embed,
    empty: () => empty$1,
    fieldset: () => fieldset,
    figcaption: () => figcaption,
    figure: () => figure,
    footer: () => footer,
    for$p: () => for$p,
    form: () => form,
    fragment: () => fragment,
    h1: () => h1,
    h2: () => h2,
    h3: () => h3,
    h4: () => h4,
    h5: () => h5,
    h6: () => h6,
    head: () => head,
    header: () => header,
    height: () => height,
    hgroup: () => hgroup,
    hidden: () => hidden,
    hr: () => hr,
    href: () => href,
    html: () => html,
    i: () => i,
    id: () => id2,
    iframe: () => iframe,
    img: () => img,
    input: () => input,
    ins: () => ins,
    kbd: () => kbd,
    keygen: () => keygen,
    label: () => label,
    lang: () => lang,
    legend: () => legend,
    li: () => li,
    list: () => list,
    main: () => main,
    map: () => map2,
    mark: () => mark,
    media: () => media,
    menu: () => menu,
    meta: () => meta,
    meter: () => meter,
    method$p: () => method$p,
    name: () => name,
    nav: () => nav,
    nbsp: () => nbsp,
    object$p: () => object$p,
    ol: () => ol,
    on: () => on$1,
    on_click: () => on_click,
    on_input: () => on_input,
    on_keydown: () => on_keydown,
    optgroup: () => optgroup,
    option: () => option,
    output: () => output,
    p: () => p,
    param: () => param,
    placeholder: () => placeholder,
    pre: () => pre,
    progress: () => progress,
    q: () => q,
    rel: () => rel,
    render: () => render,
    required: () => required,
    rows: () => rows,
    rp: () => rp,
    rt: () => rt,
    ruby: () => ruby,
    s: () => s,
    samp: () => samp,
    section: () => section,
    select: () => select,
    selected: () => selected,
    small: () => small,
    source: () => source,
    sp: () => sp,
    span: () => span,
    spellcheck: () => spellcheck,
    src: () => src,
    strong: () => strong,
    style: () => style,
    sub: () => sub4,
    summary: () => summary,
    sup: () => sup,
    tabindex: () => tabindex,
    table: () => table,
    tbody: () => tbody,
    td: () => td,
    text: () => text,
    textarea: () => textarea,
    tfoot: () => tfoot,
    th: () => th,
    thead: () => thead,
    time: () => time,
    title: () => title,
    tr: () => tr,
    track: () => track,
    type$p: () => type$p,
    u: () => u,
    ul: () => ul,
    value: () => value2,
    video: () => video,
    wbr: () => wbr,
    width: () => width,
    wrap: () => wrap
  });

  // node_modules/rescript/lib/es6/caml.js
  function i64_eq(x, y) {
    if (x[1] === y[1]) {
      return x[0] === y[0];
    } else {
      return false;
    }
  }
  function i64_ge(param2, param$1) {
    var other_hi = param$1[0];
    var hi = param2[0];
    if (hi > other_hi) {
      return true;
    } else if (hi < other_hi) {
      return false;
    } else {
      return param2[1] >= param$1[1];
    }
  }
  function i64_gt(x, y) {
    if (x[0] > y[0]) {
      return true;
    } else if (x[0] < y[0]) {
      return false;
    } else {
      return x[1] > y[1];
    }
  }
  function i64_le(x, y) {
    return !i64_gt(x, y);
  }

  // node_modules/rescript/lib/es6/caml_obj.js
  var for_in = function(o2, foo) {
    for (var x in o2) {
      foo(x);
    }
  };
  var update_dummy = function(x, y) {
    var k;
    if (Array.isArray(y)) {
      for (k = 0; k < y.length; ++k) {
        x[k] = y[k];
      }
      if (y.TAG !== void 0) {
        x.TAG = y.TAG;
      }
    } else {
      for (var k in y) {
        x[k] = y[k];
      }
    }
  };
  function equal(a2, b2) {
    if (a2 === b2) {
      return true;
    }
    var a_type = typeof a2;
    if (a_type === "string" || a_type === "number" || a_type === "boolean" || a_type === "undefined" || a2 === null) {
      return false;
    }
    var b_type = typeof b2;
    if (a_type === "function" || b_type === "function") {
      throw {
        RE_EXN_ID: "Invalid_argument",
        _1: "equal: functional value",
        Error: new Error()
      };
    }
    if (b_type === "number" || b_type === "undefined" || b2 === null) {
      return false;
    }
    var tag_a = a2.TAG | 0;
    var tag_b = b2.TAG | 0;
    if (tag_a === 248) {
      return a2[1] === b2[1];
    }
    if (tag_a === 251) {
      throw {
        RE_EXN_ID: "Invalid_argument",
        _1: "equal: abstract value",
        Error: new Error()
      };
    }
    if (tag_a !== tag_b) {
      return false;
    }
    var len_a = a2.length | 0;
    var len_b = b2.length | 0;
    if (len_a === len_b) {
      if (Array.isArray(a2)) {
        var _i = 0;
        while (true) {
          var i4 = _i;
          if (i4 === len_a) {
            return true;
          }
          if (!equal(a2[i4], b2[i4])) {
            return false;
          }
          _i = i4 + 1 | 0;
          continue;
        }
        ;
      } else if (a2 instanceof Date && b2 instanceof Date) {
        return !(a2 > b2 || a2 < b2);
      } else {
        var result = {
          contents: true
        };
        var do_key_a = function(key2) {
          if (!Object.prototype.hasOwnProperty.call(b2, key2)) {
            result.contents = false;
            return;
          }
        };
        var do_key_b = function(key2) {
          if (!Object.prototype.hasOwnProperty.call(a2, key2) || !equal(b2[key2], a2[key2])) {
            result.contents = false;
            return;
          }
        };
        for_in(a2, do_key_a);
        if (result.contents) {
          for_in(b2, do_key_b);
        }
        return result.contents;
      }
    } else {
      return false;
    }
  }

  // node_modules/rescript/lib/es6/caml_int64.js
  var min_int = [
    -2147483648,
    0
  ];
  var max_int = [
    2147483647,
    4294967295
  ];
  var one = [
    0,
    1
  ];
  var zero = [
    0,
    0
  ];
  var neg_one = [
    -1,
    4294967295
  ];
  function neg_signed(x) {
    return (x & -2147483648) !== 0;
  }
  function non_neg_signed(x) {
    return (x & -2147483648) === 0;
  }
  function neg(param2) {
    var other_lo = (param2[1] ^ -1) + 1 | 0;
    return [
      (param2[0] ^ -1) + (other_lo === 0 ? 1 : 0) | 0,
      other_lo >>> 0
    ];
  }
  function add_aux(param2, y_lo, y_hi) {
    var x_lo = param2[1];
    var lo = x_lo + y_lo | 0;
    var overflow = neg_signed(x_lo) && (neg_signed(y_lo) || non_neg_signed(lo)) || neg_signed(y_lo) && non_neg_signed(lo) ? 1 : 0;
    return [
      param2[0] + y_hi + overflow | 0,
      lo >>> 0
    ];
  }
  function add(self, param2) {
    return add_aux(self, param2[1], param2[0]);
  }
  function sub_aux(x, lo, hi) {
    var y_lo = (lo ^ -1) + 1 >>> 0;
    var y_hi = (hi ^ -1) + (y_lo === 0 ? 1 : 0) | 0;
    return add_aux(x, y_lo, y_hi);
  }
  function sub2(self, param2) {
    return sub_aux(self, param2[1], param2[0]);
  }
  function lsl_(x, numBits) {
    if (numBits === 0) {
      return x;
    }
    var lo = x[1];
    if (numBits >= 32) {
      return [
        lo << (numBits - 32 | 0),
        0
      ];
    } else {
      return [
        lo >>> (32 - numBits | 0) | x[0] << numBits,
        lo << numBits >>> 0
      ];
    }
  }
  function asr_(x, numBits) {
    if (numBits === 0) {
      return x;
    }
    var hi = x[0];
    if (numBits < 32) {
      return [
        hi >> numBits,
        (hi << (32 - numBits | 0) | x[1] >>> numBits) >>> 0
      ];
    } else {
      return [
        hi >= 0 ? 0 : -1,
        hi >> (numBits - 32 | 0) >>> 0
      ];
    }
  }
  function is_zero(param2) {
    if (param2[0] !== 0) {
      return false;
    } else {
      return param2[1] === 0;
    }
  }
  function mul(_this, _other) {
    while (true) {
      var other = _other;
      var $$this3 = _this;
      var lo;
      var this_hi = $$this3[0];
      var exit = 0;
      var exit$1 = 0;
      var exit$2 = 0;
      if (this_hi !== 0) {
        exit$2 = 4;
      } else {
        if ($$this3[1] === 0) {
          return zero;
        }
        exit$2 = 4;
      }
      if (exit$2 === 4) {
        if (other[0] !== 0) {
          exit$1 = 3;
        } else {
          if (other[1] === 0) {
            return zero;
          }
          exit$1 = 3;
        }
      }
      if (exit$1 === 3) {
        if (this_hi !== -2147483648 || $$this3[1] !== 0) {
          exit = 2;
        } else {
          lo = other[1];
        }
      }
      if (exit === 2) {
        var other_hi = other[0];
        var lo$1 = $$this3[1];
        var exit$3 = 0;
        if (other_hi !== -2147483648 || other[1] !== 0) {
          exit$3 = 3;
        } else {
          lo = lo$1;
        }
        if (exit$3 === 3) {
          var other_lo = other[1];
          if (this_hi < 0) {
            if (other_hi >= 0) {
              return neg(mul(neg($$this3), other));
            }
            _other = neg(other);
            _this = neg($$this3);
            continue;
          }
          if (other_hi < 0) {
            return neg(mul($$this3, neg(other)));
          }
          var a48 = this_hi >>> 16;
          var a32 = this_hi & 65535;
          var a16 = lo$1 >>> 16;
          var a00 = lo$1 & 65535;
          var b48 = other_hi >>> 16;
          var b32 = other_hi & 65535;
          var b16 = other_lo >>> 16;
          var b00 = other_lo & 65535;
          var c48 = 0;
          var c32 = 0;
          var c16 = 0;
          var c00 = a00 * b00;
          c16 = (c00 >>> 16) + a16 * b00;
          c32 = c16 >>> 16;
          c16 = (c16 & 65535) + a00 * b16;
          c32 = c32 + (c16 >>> 16) + a32 * b00;
          c48 = c32 >>> 16;
          c32 = (c32 & 65535) + a16 * b16;
          c48 = c48 + (c32 >>> 16);
          c32 = (c32 & 65535) + a00 * b32;
          c48 = c48 + (c32 >>> 16);
          c32 = c32 & 65535;
          c48 = c48 + (a48 * b00 + a32 * b16 + a16 * b32 + a00 * b48) & 65535;
          return [
            c32 | c48 << 16,
            (c00 & 65535 | (c16 & 65535) << 16) >>> 0
          ];
        }
      }
      if ((lo & 1) === 0) {
        return zero;
      } else {
        return min_int;
      }
    }
    ;
  }
  function or_(param2, param$1) {
    return [
      param2[0] | param$1[0],
      (param2[1] | param$1[1]) >>> 0
    ];
  }
  function to_float(param2) {
    return param2[0] * 4294967296 + param2[1];
  }
  function of_float(x) {
    if (isNaN(x) || !isFinite(x)) {
      return zero;
    }
    if (x <= -9223372036854776e3) {
      return min_int;
    }
    if (x + 1 >= 9223372036854776e3) {
      return max_int;
    }
    if (x < 0) {
      return neg(of_float(-x));
    }
    var hi = x / 4294967296 | 0;
    var lo = x % 4294967296 | 0;
    return [
      hi,
      lo >>> 0
    ];
  }
  function div(_self, _other) {
    while (true) {
      var other = _other;
      var self = _self;
      var self_hi = self[0];
      var exit = 0;
      var exit$1 = 0;
      if (other[0] !== 0 || other[1] !== 0) {
        exit$1 = 2;
      } else {
        throw {
          RE_EXN_ID: "Division_by_zero",
          Error: new Error()
        };
      }
      if (exit$1 === 2) {
        if (self_hi !== -2147483648) {
          if (self_hi !== 0) {
            exit = 1;
          } else {
            if (self[1] === 0) {
              return zero;
            }
            exit = 1;
          }
        } else if (self[1] !== 0) {
          exit = 1;
        } else {
          if (i64_eq(other, one) || i64_eq(other, neg_one)) {
            return self;
          }
          if (i64_eq(other, min_int)) {
            return one;
          }
          var half_this = asr_(self, 1);
          var approx = lsl_(div(half_this, other), 1);
          var exit$2 = 0;
          if (approx[0] !== 0) {
            exit$2 = 3;
          } else {
            if (approx[1] === 0) {
              if (other[0] < 0) {
                return one;
              } else {
                return neg(one);
              }
            }
            exit$2 = 3;
          }
          if (exit$2 === 3) {
            var rem = sub2(self, mul(other, approx));
            return add(approx, div(rem, other));
          }
        }
      }
      if (exit === 1) {
        var other_hi = other[0];
        var exit$3 = 0;
        if (other_hi !== -2147483648) {
          exit$3 = 2;
        } else {
          if (other[1] === 0) {
            return zero;
          }
          exit$3 = 2;
        }
        if (exit$3 === 2) {
          if (self_hi < 0) {
            if (other_hi >= 0) {
              return neg(div(neg(self), other));
            }
            _other = neg(other);
            _self = neg(self);
            continue;
          }
          if (other_hi < 0) {
            return neg(div(self, neg(other)));
          }
          var res = zero;
          var rem$1 = self;
          while (i64_ge(rem$1, other)) {
            var b2 = Math.floor(to_float(rem$1) / to_float(other));
            var approx$1 = 1 > b2 ? 1 : b2;
            var log2 = Math.ceil(Math.log(approx$1) / Math.LN2);
            var delta = log2 <= 48 ? 1 : Math.pow(2, log2 - 48);
            var approxRes = of_float(approx$1);
            var approxRem = mul(approxRes, other);
            while (approxRem[0] < 0 || i64_gt(approxRem, rem$1)) {
              approx$1 = approx$1 - delta;
              approxRes = of_float(approx$1);
              approxRem = mul(approxRes, other);
            }
            ;
            if (is_zero(approxRes)) {
              approxRes = one;
            }
            res = add(res, approxRes);
            rem$1 = sub2(rem$1, approxRem);
          }
          ;
          return res;
        }
      }
    }
    ;
  }
  function mod_(self, other) {
    return sub2(self, mul(div(self, other), other));
  }
  function of_int32(lo) {
    return [
      lo < 0 ? -1 : 0,
      lo >>> 0
    ];
  }

  // node_modules/rescript/lib/es6/caml_string.js
  function get2(s3, i4) {
    if (i4 >= s3.length || i4 < 0) {
      throw {
        RE_EXN_ID: "Invalid_argument",
        _1: "index out of bounds",
        Error: new Error()
      };
    }
    return s3.codePointAt(i4);
  }

  // node_modules/rescript/lib/es6/caml_exceptions.js
  var id = {
    contents: 0
  };
  function create(str) {
    id.contents = id.contents + 1 | 0;
    return str + ("/" + id.contents);
  }

  // node_modules/rescript/lib/es6/caml_option.js
  function some(x) {
    if (x === void 0) {
      return {
        BS_PRIVATE_NESTED_SOME_NONE: 0
      };
    } else if (x !== null && x.BS_PRIVATE_NESTED_SOME_NONE !== void 0) {
      return {
        BS_PRIVATE_NESTED_SOME_NONE: x.BS_PRIVATE_NESTED_SOME_NONE + 1 | 0
      };
    } else {
      return x;
    }
  }
  function valFromOption(x) {
    if (!(x !== null && x.BS_PRIVATE_NESTED_SOME_NONE !== void 0)) {
      return x;
    }
    var depth = x.BS_PRIVATE_NESTED_SOME_NONE;
    if (depth === 0) {
      return;
    } else {
      return {
        BS_PRIVATE_NESTED_SOME_NONE: depth - 1 | 0
      };
    }
  }

  // node_modules/rescript/lib/es6/pervasives.js
  function failwith(s3) {
    throw {
      RE_EXN_ID: "Failure",
      _1: s3,
      Error: new Error()
    };
  }
  function invalid_arg(s3) {
    throw {
      RE_EXN_ID: "Invalid_argument",
      _1: s3,
      Error: new Error()
    };
  }
  function $at(l1, l2) {
    if (l1) {
      return {
        hd: l1.hd,
        tl: $at(l1.tl, l2)
      };
    } else {
      return l2;
    }
  }

  // node_modules/rescript/lib/es6/list.js
  function length(l) {
    var _len = 0;
    var _param = l;
    while (true) {
      var param2 = _param;
      var len = _len;
      if (!param2) {
        return len;
      }
      _param = param2.tl;
      _len = len + 1 | 0;
      continue;
    }
    ;
  }
  function iter(f2, _param) {
    while (true) {
      var param2 = _param;
      if (!param2) {
        return;
      }
      _1(f2, param2.hd);
      _param = param2.tl;
      continue;
    }
    ;
  }
  function fold_left(f2, _accu, _l) {
    while (true) {
      var l = _l;
      var accu = _accu;
      if (!l) {
        return accu;
      }
      _l = l.tl;
      _accu = _2(f2, accu, l.hd);
      continue;
    }
    ;
  }

  // lib/es6/src/helix-rescript/Metajs_external.bs.js
  var $$null = null;
  var $$undefined = void 0;
  var new_obj = function(c, a2) {
    return new c(...a2);
  };
  var get3 = function(obj2, prop) {
    return obj2[prop];
  };
  var set2 = function(obj2, prop, value3) {
    obj2[prop] = value3;
  };
  var meth_call = function(o2, n, a2) {
    return o2[n].apply(o2, a2);
  };
  function callback(param2, f2) {
    return f2;
  }
  var $$instanceof = function(obj2, cls) {
    return obj2 instanceof cls;
  };

  // lib/es6/src/metajs/Metajs.bs.js
  function is_none(v) {
    if (v === $$null) {
      return true;
    } else {
      return v === $$undefined;
    }
  }
  function option_of_js(of_js, v) {
    if (is_none(v)) {
      return;
    } else {
      return some(_1(of_js, v));
    }
  }
  function meth_call_unit($$this3, f2, args) {
    meth_call($$this3, f2, args);
  }
  var $$this = globalThis;
  var $$window = get3($$this, "window");
  var $$document = get3($$this, "document");
  var $$console = get3($$this, "console");
  var $$global = globalThis;
  function js_of_string(prim) {
    return prim;
  }
  function string_of_js(prim) {
    return prim;
  }
  function bool_of_js(prim) {
    return prim;
  }
  function float_of_js(prim) {
    return prim;
  }
  function js_of_int(prim) {
    return prim;
  }
  function int_of_js(prim) {
    return prim;
  }
  var new_obj2 = new_obj;
  var get4 = get3;
  var set3 = set2;
  var meth_call2 = meth_call;
  var callback2 = callback;
  var $$instanceof2 = $$instanceof;
  function repr(prim) {
    return prim;
  }
  var Global = {
    $$this,
    $$document,
    $$window,
    $$console
  };

  // lib/es6/src/stdweb/Stdweb.bs.js
  function to_string2(t2) {
    return t2;
  }
  var Kind = {
    to_string: to_string2
  };
  function target(t2) {
    return get4(t2, "target");
  }
  function checked($$this3) {
    return bool_of_js(get4($$this3, "checked"));
  }
  function value($$this3) {
    return string_of_js(get4($$this3, "value"));
  }
  function set_value($$this3, value3) {
    set3($$this3, "value", js_of_string(value3));
  }
  function target_value(ev) {
    var $$this3 = get4(ev, "target");
    return string_of_js(get4($$this3, "value"));
  }
  function data($$this3) {
    return string_of_js(get4($$this3, "data"));
  }
  var Input = {
    data
  };
  function key($$this3) {
    return string_of_js(get4($$this3, "key"));
  }
  function code($$this3) {
    return string_of_js(get4($$this3, "keyCode"));
  }
  var Keyboard = {
    key,
    code
  };
  function page_x($$this3) {
    return float_of_js(get4($$this3, "pageX"));
  }
  function page_y($$this3) {
    return float_of_js(get4($$this3, "pageY"));
  }
  var Mouse = {
    page_x,
    page_y
  };
  function add_event_listener($$this3, event_name, f2) {
    meth_call_unit($$this3, "addEventListener", [
      js_of_string(event_name),
      callback2(1, f2)
    ]);
  }
  function remove_event_listener($$this3, event_name) {
    meth_call_unit($$this3, "removeEventLister", [js_of_string(event_name)]);
  }
  var Event_target = {
    add_event_listener,
    remove_event_listener
  };
  function for_each($$this3, f2) {
    meth_call_unit($$this3, "forEach", [callback2(1, f2)]);
  }
  var List = {
    for_each
  };
  function as_js(t2) {
    return t2;
  }
  function as_event_target(t2) {
    return t2;
  }
  function parent_node($$this3) {
    return option_of_js(function(x) {
      return x;
    }, get4($$this3, "parentNode"));
  }
  function child_nodes($$this3) {
    return get4($$this3, "childNodes");
  }
  function first_child($$this3) {
    return option_of_js(function(x) {
      return x;
    }, get4($$this3, "firstChild"));
  }
  function last_child($$this3) {
    return option_of_js(function(x) {
      return x;
    }, get4($$this3, "lastChild"));
  }
  function next_sibling($$this3) {
    return option_of_js(function(x) {
      return x;
    }, get4($$this3, "nextSibling"));
  }
  function append_child(parent, other) {
    meth_call_unit(parent, "appendChild", [other]);
  }
  function remove_child(parent, other) {
    meth_call_unit(parent, "removeChild", [other]);
  }
  function insert_before(parent, reference, new_node) {
    meth_call_unit(parent, "insertBefore", [
      new_node,
      reference
    ]);
  }
  function replace_child(parent, reference, new_node) {
    meth_call_unit(parent, "replaceChild", [
      new_node,
      reference
    ]);
  }
  function set_text_content($$this3, text2) {
    set3($$this3, "textContent", js_of_string(text2));
  }
  function get_text_content($$this3) {
    return string_of_js(get4($$this3, "textContent"));
  }
  function is_same_node($$this3, other) {
    return bool_of_js(meth_call2($$this3, "isSameNode", [other]));
  }
  function as_node(t2) {
    return t2;
  }
  function replace_children($$this3, children) {
    meth_call_unit($$this3, "replaceChildren", children);
  }
  function append($$this3, other) {
    meth_call_unit($$this3, "append", [other]);
  }
  function replace_with($$this3, other) {
    meth_call_unit($$this3, "replaceWith", [other]);
  }
  function set_attribute($$this3, name2, value3) {
    meth_call_unit($$this3, "setAttribute", [
      js_of_string(name2),
      js_of_string(value3)
    ]);
  }
  function remove_attribute($$this3, name2) {
    meth_call_unit($$this3, "removeAttribute", [js_of_string(name2)]);
  }
  function css_text($$this3) {
    return string_of_js(get4($$this3, "cssText"));
  }
  function length2($$this3) {
    return int_of_js(get4($$this3, "length"));
  }
  function set_property($$this3, name2, value3) {
    meth_call_unit($$this3, "setProperty", [
      js_of_string(name2),
      js_of_string(value3)
    ]);
  }
  function get_property($$this3, name2) {
    return string_of_js(meth_call2($$this3, "setProperty", [js_of_string(name2)]));
  }
  function remove_property($$this3, name2) {
    meth_call_unit($$this3, "removeProperty", [js_of_string(name2)]);
  }
  var Css_style_declaration = {
    css_text,
    length: length2,
    set_property,
    get_property,
    remove_property
  };
  function of_node(t2) {
    return t2;
  }
  function of_element(t2) {
    return t2;
  }
  function as_element(t2) {
    return t2;
  }
  function get_style($$this3) {
    return get4($$this3, "style");
  }
  function set_style_property($$this3, name2, value3) {
    set_property(get4($$this3, "style"), name2, value3);
  }
  function get_style_property($$this3, name2) {
    return get_property(get4($$this3, "style"), name2);
  }
  function remove_style_property($$this3, name2) {
    remove_property(get4($$this3, "style"), name2);
  }
  function as_node$1(t2) {
    return t2;
  }
  var Character_data = {
    as_node: as_node$1
  };
  var t = get4($$global, "Text");
  function as_character_data(t2) {
    return t2;
  }
  var $$Text = {
    as_node: as_node$1,
    t,
    as_character_data
  };
  var t$1 = get4($$global, "Comment");
  function as_character_data$1(t2) {
    return t2;
  }
  function make2(data2) {
    return new_obj2(t$1, [js_of_string(data2)]);
  }
  var t$2 = get4($$global, "DocumentFragment");
  function as_node$2(t2) {
    return t2;
  }
  function make$1(param2) {
    return new_obj2(t$2, []);
  }
  function replace_children$1($$this3, children) {
    meth_call_unit($$this3, "replaceChildren", children);
  }
  var Document_fragment = {
    t: t$2,
    as_node: as_node$2,
    make: make$1,
    replace_children: replace_children$1
  };
  var $$this2 = Global.$$document;
  function as_node$3($$this3) {
    return $$this3;
  }
  function get_element_by_id(id3) {
    return option_of_js(function(x) {
      return x;
    }, meth_call2(Global.$$document, "getElementById", [js_of_string(id3)]));
  }
  function create_text_node(text2) {
    return meth_call2(Global.$$document, "createTextNode", [js_of_string(text2)]);
  }
  function create_element(name2) {
    return meth_call2(Global.$$document, "createElement", [js_of_string(name2)]);
  }
  var $$this$1 = Global.$$window;
  function as_event_target$1(t2) {
    return t2;
  }
  function set_interval(f2, ms) {
    meth_call_unit(Global.$$window, "setInterval", [
      callback2(1, f2),
      js_of_int(ms)
    ]);
  }
  function set_timeout(f2, ms) {
    meth_call_unit(Global.$$window, "setTimeout", [
      callback2(1, f2),
      js_of_int(ms)
    ]);
  }
  var $$Window = {
    $$this: $$this$1,
    add_event_listener,
    remove_event_listener,
    as_event_target: as_event_target$1,
    set_interval,
    set_timeout
  };
  function log(x) {
    meth_call_unit(Global.$$console, "log", [repr(x)]);
  }
  var Console = {
    log
  };
  var Dom_Event = {
    Kind,
    target,
    target_value,
    Target: {
      value,
      set_value,
      checked
    },
    Input,
    Keyboard,
    Mouse,
    click: "click",
    input: "input",
    keydown: "keydown",
    change: "change"
  };
  var Dom_Node = {
    List,
    add_event_listener,
    remove_event_listener,
    as_js,
    as_event_target,
    parent_node,
    child_nodes,
    first_child,
    last_child,
    next_sibling,
    append_child,
    insert_before,
    replace_child,
    remove_child,
    set_text_content,
    get_text_content,
    is_same_node
  };
  var Dom_Element = {
    List,
    add_event_listener,
    remove_event_listener,
    as_js,
    as_event_target,
    parent_node,
    child_nodes,
    first_child,
    last_child,
    next_sibling,
    append_child,
    insert_before,
    replace_child,
    remove_child,
    set_text_content,
    get_text_content,
    is_same_node,
    as_node,
    append,
    replace_with,
    set_attribute,
    remove_attribute,
    replace_children
  };
  var Dom_Html_element = {
    of_element,
    of_node,
    as_element,
    get_style,
    set_style_property,
    get_style_property,
    remove_style_property
  };
  var Dom_Comment = {
    as_node: as_node$1,
    as_character_data: as_character_data$1,
    make: make2
  };
  var Dom_Document = {
    $$this: $$this2,
    as_node: as_node$3,
    get_element_by_id,
    create_element,
    create_text_node
  };
  var Dom = {
    $$Event: Dom_Event,
    Event_target,
    $$Node: Dom_Node,
    $$Element: Dom_Element,
    Css_style_declaration,
    Html_element: Dom_Html_element,
    Character_data,
    $$Text,
    $$Comment: Dom_Comment,
    Document_fragment,
    $$Document: Dom_Document,
    $$Window
  };

  // node_modules/rescript/lib/es6/belt_List.js
  function length3(xs) {
    var _x = xs;
    var _acc = 0;
    while (true) {
      var acc = _acc;
      var x = _x;
      if (!x) {
        return acc;
      }
      _acc = acc + 1 | 0;
      _x = x.tl;
      continue;
    }
    ;
  }
  function fillAux(arr, _i, _x) {
    while (true) {
      var x = _x;
      var i4 = _i;
      if (!x) {
        return;
      }
      arr[i4] = x.hd;
      _x = x.tl;
      _i = i4 + 1 | 0;
      continue;
    }
    ;
  }
  function toArray(x) {
    var len = length3(x);
    var arr = new Array(len);
    fillAux(arr, 0, x);
    return arr;
  }

  // node_modules/rescript/lib/es6/string.js
  function concat(sep, xs) {
    return toArray(xs).join(sep);
  }

  // lib/es6/src/html/Html.bs.js
  function empty_set(param2) {
  }
  function empty_remove(param2) {
  }
  var empty = {
    set: empty_set,
    remove: empty_remove
  };
  function on(bool3, attr2) {
    if (bool3) {
      return attr2;
    } else {
      return empty;
    }
  }
  function on_some(at) {
    if (at !== void 0) {
      return at;
    } else {
      return empty;
    }
  }
  function on_ok(attr2) {
    if (attr2.TAG === 0) {
      return attr2._0;
    } else {
      return empty;
    }
  }
  function string(name2, value3) {
    return {
      set: function(el) {
        _3(Dom.$$Element.set_attribute, el, name2, value3);
      },
      remove: function(el) {
        _2(Dom.$$Element.remove_attribute, el, name2);
      }
    };
  }
  function bool(name2, bool$1) {
    if (bool$1) {
      return string(name2, "");
    } else {
      return empty;
    }
  }
  function $$int(name2, i4) {
    return string(name2, String(i4));
  }
  function elem(f2) {
    return {
      set: function(elem2) {
        _1(f2, _1(Dom.$$Element.as_node, elem2));
      },
      remove: function(param2) {
      }
    };
  }
  function bind(r) {
    return {
      set: function(elem2) {
        r.contents = some(_1(Dom.$$Element.as_node, elem2));
      },
      remove: function(param2) {
      }
    };
  }
  function of_attr(x) {
    return x;
  }
  function to_attr(x) {
    return x;
  }
  var Internal = {
    of_attr,
    to_attr
  };
  function accept(value3) {
    return string("accept", value3);
  }
  function accesskey(value3) {
    return string("accesskey", value3);
  }
  function action(value3) {
    return string("action", value3);
  }
  function autocomplete(value3) {
    return string("autocomplete", value3);
  }
  var autofocus = string("autofocus", "");
  function charset(value3) {
    return string("charset", value3);
  }
  var checked2 = string("checked", "");
  function class_name(value3) {
    return string("class", value3);
  }
  function cols(value3) {
    return string("cols", String(value3));
  }
  function content(value3) {
    return string("content", value3);
  }
  function contenteditable(value3) {
    return bool("contenteditable", value3);
  }
  var defer = string("defer", "");
  function dir(value3) {
    return string("dir", value3);
  }
  var disabled = string("disabled", "");
  function draggable(value3) {
    return bool("draggable", value3);
  }
  function for$p(value3) {
    return string("for", value3);
  }
  function height(value3) {
    return string("height", String(value3));
  }
  var hidden = string("hidden", "");
  function href(value3) {
    return string("href", value3);
  }
  function id2(value3) {
    return string("id", value3);
  }
  function lang(value3) {
    return string("lang", value3);
  }
  function list(value3) {
    return string("list", value3);
  }
  function media(value3) {
    return string("media", value3);
  }
  function method$p(value3) {
    return string("method", value3);
  }
  function name(value3) {
    return string("name", value3);
  }
  function placeholder(value3) {
    return string("placeholder", value3);
  }
  function rel(value3) {
    return string("rel", value3);
  }
  var required = string("required", "");
  function rows(value3) {
    return string("rows", String(value3));
  }
  var selected = string("selected", "");
  function spellcheck(value3) {
    return string("spellcheck", value3);
  }
  function src(value3) {
    return string("src", value3);
  }
  function tabindex(value3) {
    return string("tabindex", String(value3));
  }
  function title(value3) {
    return string("title", value3);
  }
  function type$p(value3) {
    return string("type", value3);
  }
  function value2(value$1) {
    return string("value", value$1);
  }
  function wrap(value3) {
    return string("wrap", value3);
  }
  function width(value3) {
    return string("width", String(value3));
  }
  function style(items) {
    var set5 = function(elem2) {
      var html_elem = _1(Dom.Html_element.of_element, elem2);
      iter(function(param2) {
        _3(Dom.Html_element.set_style_property, html_elem, param2[0], param2[1]);
      }, items);
    };
    var remove2 = function(elem2) {
      var html_elem = _1(Dom.Html_element.of_element, elem2);
      iter(function(param2) {
        _2(Dom.Html_element.remove_style_property, html_elem, param2[0]);
      }, items);
    };
    return {
      set: set5,
      remove: remove2
    };
  }
  function class_list(xs) {
    var value3 = concat(" ", xs);
    return string("class", value3);
  }
  function class_flags(options) {
    return class_list(fold_left(function(acc, param2) {
      if (param2[1]) {
        return {
          hd: param2[0],
          tl: acc
        };
      } else {
        return acc;
      }
    }, 0, options));
  }
  function on$1(kind, f2) {
    var kind$1 = _1(Dom.$$Event.Kind.to_string, kind);
    return {
      set: function(el) {
        _3(Dom.$$Element.add_event_listener, el, kind$1, f2);
      },
      remove: function(el) {
        _2(Dom.$$Element.remove_event_listener, el, kind$1);
      }
    };
  }
  function on_click(param2) {
    return on$1(Dom.$$Event.click, param2);
  }
  function on_input(param2) {
    return on$1(Dom.$$Event.input, param2);
  }
  function on_keydown(param2) {
    return on$1(Dom.$$Event.keydown, param2);
  }
  function elem$1(name2, attrs, children) {
    var el = _1(Dom.$$Document.create_element, name2);
    var node = _1(Dom.$$Element.as_node, el);
    iter(function(param2) {
      return _1(param2.set, el);
    }, attrs);
    iter(function(child) {
      _2(Dom.$$Node.append_child, node, child);
    }, children);
    return node;
  }
  function text(data2) {
    return _1(Dom.$$Text.as_node, _1(Dom.$$Document.create_text_node, data2));
  }
  function $$int$1(n) {
    return text(String(n));
  }
  var empty$1 = _1(Dom.$$Comment.as_node, _1(Dom.$$Comment.make, "empty"));
  var sp = text(" ");
  var nbsp = text("\xC2\xA0");
  function fragment(children) {
    var node = _1(Dom.Document_fragment.as_node, _1(Dom.Document_fragment.make, void 0));
    iter(function(child) {
      _2(Dom.$$Node.append_child, node, child);
    }, children);
    return node;
  }
  function a(attrs, children) {
    return elem$1("a", attrs, children);
  }
  function abbr(attrs, children) {
    return elem$1("abbr", attrs, children);
  }
  function address(attrs, children) {
    return elem$1("address", attrs, children);
  }
  function area(attrs) {
    return elem$1("area", attrs, 0);
  }
  function article(attrs, children) {
    return elem$1("article", attrs, children);
  }
  function aside(attrs, children) {
    return elem$1("aside", attrs, children);
  }
  function audio(attrs, children) {
    return elem$1("audio", attrs, children);
  }
  function b(attrs, children) {
    return elem$1("b", attrs, children);
  }
  function base(attrs) {
    return elem$1("base", attrs, 0);
  }
  function bdi(attrs, children) {
    return elem$1("bdi", attrs, children);
  }
  function bdo(attrs, children) {
    return elem$1("bdo", attrs, children);
  }
  function blockquote(attrs, children) {
    return elem$1("blockquote", attrs, children);
  }
  function br(attrs) {
    return elem$1("br", attrs, 0);
  }
  function button(attrs, children) {
    return elem$1("button", attrs, children);
  }
  function canvas(attrs, children) {
    return elem$1("canvas", attrs, children);
  }
  function caption(attrs, children) {
    return elem$1("caption", attrs, children);
  }
  function cite(attrs, children) {
    return elem$1("cite", attrs, children);
  }
  function code2(attrs, data2) {
    return elem$1("code", attrs, {
      hd: text(data2),
      tl: 0
    });
  }
  function col(attrs) {
    return elem$1("col", attrs, 0);
  }
  function colgroup(attrs, children) {
    return elem$1("colgroup", attrs, children);
  }
  function command(attrs, children) {
    return elem$1("command", attrs, children);
  }
  function datalist(attrs, children) {
    return elem$1("datalist", attrs, children);
  }
  function dd(attrs, children) {
    return elem$1("dd", attrs, children);
  }
  function del(attrs, children) {
    return elem$1("del", attrs, children);
  }
  function details(attrs, children) {
    return elem$1("details", attrs, children);
  }
  function dfn(attrs, children) {
    return elem$1("dfn", attrs, children);
  }
  function div2(attrs, children) {
    return elem$1("div", attrs, children);
  }
  function dl(attrs, children) {
    return elem$1("dl", attrs, children);
  }
  function dt(attrs, children) {
    return elem$1("dt", attrs, children);
  }
  function em(attrs, children) {
    return elem$1("em", attrs, children);
  }
  function embed(attrs) {
    return elem$1("embed", attrs, 0);
  }
  function fieldset(attrs, children) {
    return elem$1("fieldset", attrs, children);
  }
  function figcaption(attrs, children) {
    return elem$1("figcaption", attrs, children);
  }
  function figure(attrs, children) {
    return elem$1("figure", attrs, children);
  }
  function footer(attrs, children) {
    return elem$1("footer", attrs, children);
  }
  function form(attrs, children) {
    return elem$1("form", attrs, children);
  }
  function h1(attrs, children) {
    return elem$1("h1", attrs, children);
  }
  function h2(attrs, children) {
    return elem$1("h2", attrs, children);
  }
  function h3(attrs, children) {
    return elem$1("h3", attrs, children);
  }
  function h4(attrs, children) {
    return elem$1("h4", attrs, children);
  }
  function h5(attrs, children) {
    return elem$1("h5", attrs, children);
  }
  function h6(attrs, children) {
    return elem$1("h6", attrs, children);
  }
  function head(attrs, children) {
    return elem$1("head", attrs, children);
  }
  function header(attrs, children) {
    return elem$1("header", attrs, children);
  }
  function hgroup(attrs, children) {
    return elem$1("hgroup", attrs, children);
  }
  function hr(attrs) {
    return elem$1("hr", attrs, 0);
  }
  function html(attrs, children) {
    return elem$1("html", attrs, children);
  }
  function i(attrs, children) {
    return elem$1("i", attrs, children);
  }
  function iframe(attrs, children) {
    return elem$1("iframe", attrs, children);
  }
  function img(attrs) {
    return elem$1("img", attrs, 0);
  }
  function input(attrs) {
    return elem$1("input", attrs, 0);
  }
  function ins(attrs, children) {
    return elem$1("ins", attrs, children);
  }
  function kbd(attrs, children) {
    return elem$1("kbd", attrs, children);
  }
  function keygen(attrs, children) {
    return elem$1("keygen", attrs, children);
  }
  function label(attrs, children) {
    return elem$1("label", attrs, children);
  }
  function legend(attrs, children) {
    return elem$1("legend", attrs, children);
  }
  function li(attrs, children) {
    return elem$1("li", attrs, children);
  }
  function main(attrs, children) {
    return elem$1("main", attrs, children);
  }
  function map2(attrs, children) {
    return elem$1("map", attrs, children);
  }
  function mark(attrs, children) {
    return elem$1("mark", attrs, children);
  }
  function menu(attrs, children) {
    return elem$1("menu", attrs, children);
  }
  function meta(attrs) {
    return elem$1("meta", attrs, 0);
  }
  function meter(attrs, children) {
    return elem$1("meter", attrs, children);
  }
  function nav(attrs, children) {
    return elem$1("nav", attrs, children);
  }
  function object$p(attrs, children) {
    return elem$1("object", attrs, children);
  }
  function ol(attrs, children) {
    return elem$1("ol", attrs, children);
  }
  function optgroup(attrs, children) {
    return elem$1("optgroup", attrs, children);
  }
  function option(attrs, children) {
    return elem$1("option", attrs, children);
  }
  function output(attrs, children) {
    return elem$1("output", attrs, children);
  }
  function p(attrs, children) {
    return elem$1("p", attrs, children);
  }
  function param(attrs) {
    return elem$1("param", attrs, 0);
  }
  function pre(attrs, children) {
    return elem$1("pre", attrs, children);
  }
  function progress(attrs, children) {
    return elem$1("progress", attrs, children);
  }
  function q(attrs, children) {
    return elem$1("q", attrs, children);
  }
  function rp(attrs, children) {
    return elem$1("rp", attrs, children);
  }
  function rt(attrs, children) {
    return elem$1("rt", attrs, children);
  }
  function ruby(attrs, children) {
    return elem$1("ruby", attrs, children);
  }
  function s(attrs, children) {
    return elem$1("s", attrs, children);
  }
  function samp(attrs, children) {
    return elem$1("samp", attrs, children);
  }
  function section(attrs, children) {
    return elem$1("section", attrs, children);
  }
  function select(attrs, children) {
    return elem$1("select", attrs, children);
  }
  function small(attrs, children) {
    return elem$1("small", attrs, children);
  }
  function source(attrs) {
    return elem$1("source", attrs, 0);
  }
  function span(attrs, children) {
    return elem$1("span", attrs, children);
  }
  function strong(attrs, children) {
    return elem$1("strong", attrs, children);
  }
  function sub4(attrs, children) {
    return elem$1("sub", attrs, children);
  }
  function summary(attrs, children) {
    return elem$1("summary", attrs, children);
  }
  function sup(attrs, children) {
    return elem$1("sup", attrs, children);
  }
  function table(attrs, children) {
    return elem$1("table", attrs, children);
  }
  function tbody(attrs, children) {
    return elem$1("tbody", attrs, children);
  }
  function td(attrs, children) {
    return elem$1("td", attrs, children);
  }
  function textarea(attrs, children) {
    return elem$1("textarea", attrs, children);
  }
  function tfoot(attrs, children) {
    return elem$1("tfoot", attrs, children);
  }
  function th(attrs, children) {
    return elem$1("th", attrs, children);
  }
  function thead(attrs, children) {
    return elem$1("thead", attrs, children);
  }
  function time(attrs, children) {
    return elem$1("time", attrs, children);
  }
  function tr(attrs, children) {
    return elem$1("tr", attrs, children);
  }
  function track(attrs) {
    return elem$1("track", attrs, 0);
  }
  function u(attrs, children) {
    return elem$1("u", attrs, children);
  }
  function ul(attrs, children) {
    return elem$1("ul", attrs, children);
  }
  function $$var(attrs, children) {
    return elem$1("var", attrs, children);
  }
  function video(attrs, children) {
    return elem$1("video", attrs, children);
  }
  function wbr(attrs) {
    return elem$1("wbr", attrs, 0);
  }
  function of_some(to_html2, option2) {
    if (option2 !== void 0) {
      return _1(to_html2, valFromOption(option2));
    } else {
      return empty$1;
    }
  }
  function of_ok(to_html2, result) {
    if (result.TAG === 0) {
      return _1(to_html2, result._0);
    } else {
      return empty$1;
    }
  }
  function of_html(x) {
    return x;
  }
  function to_html(x) {
    return x;
  }
  var Internal$1 = {
    of_html,
    to_html
  };
  var $$Node = {
    of_some,
    of_ok,
    Internal: Internal$1
  };
  function render(parent, node) {
    _2(Dom.$$Node.append_child, _1(Dom.$$Element.as_node, parent), node);
  }
  var attr = string;
  var $$Attr = {
    empty,
    bool,
    $$int,
    on,
    on_some,
    on_ok,
    elem,
    bind,
    Internal
  };

  // lib/es6/src/signal/Signal.bs.js
  var Signal_bs_exports = {};
  __export(Signal_bs_exports, {
    $$const: () => $$const,
    apply: () => apply,
    emit: () => emit$1,
    emitter: () => emitter,
    filter: () => filter,
    filter_map: () => filter_map,
    get: () => get5,
    make: () => make4,
    map: () => map3,
    map2: () => map22,
    map3: () => map32,
    never: () => never,
    pair: () => pair,
    reduce: () => reduce,
    reducer: () => reducer,
    sample: () => sample,
    select: () => select2,
    sub: () => sub$1,
    sub2: () => sub22,
    tap: () => tap,
    triple: () => triple,
    uniq: () => uniq,
    update: () => update,
    use: () => use,
    use2: () => use2
  });
  function add2(k, subs) {
    subs.contents = $at(subs.contents, {
      hd: k,
      tl: 0
    });
  }
  function dispatch(x, subs) {
    iter(function(k) {
      _1(k, x);
    }, subs.contents);
  }
  function base2(name2, value3) {
    var subs = {
      contents: 0
    };
    var sub6 = function(k) {
      add2(k, subs);
    };
    var s3 = {};
    var emit2 = function(x) {
      s3.value = x;
      dispatch(s3.value, subs);
    };
    update_dummy(s3, {
      name: name2,
      value: value3,
      emit: emit2,
      sub: sub6
    });
    return s3;
  }
  function make4(value3) {
    return base2("make", value3);
  }
  function sub5(param2) {
  }
  function emit(_x) {
  }
  var s2 = {
    name: "never",
    value: void 0,
    emit,
    sub: sub5
  };
  function get5(s3) {
    return s3.value;
  }
  function emit$1(x, s3) {
    _1(s3.emit, x);
  }
  function update(f2, s3) {
    _1(s3.emit, _1(f2, s3.value));
  }
  function sub$1(k, s3) {
    _1(s3.sub, k);
  }
  function sub22(k, s1, s22) {
    _1(s1.sub, function(x1) {
      _2(k, x1, s22.value);
    });
    _1(s22.sub, function(x2) {
      _2(k, s1.value, x2);
    });
  }
  function use(k, s3) {
    _1(s3.sub, k);
    _1(k, s3.value);
  }
  function use2(k, s1, s22) {
    _1(s1.sub, function(x1) {
      _2(k, x1, s22.value);
    });
    _1(s22.sub, function(x2) {
      _2(k, s1.value, x2);
    });
    _2(k, s1.value, s22.value);
  }
  function map3(f2, s3) {
    var s$p = base2("map", _1(f2, s3.value));
    _1(s3.sub, function(x) {
      _1(s$p.emit, _1(f2, x));
    });
    return s$p;
  }
  function $$const(x, s3) {
    var s$p = base2("const", x);
    _1(s3.sub, function(param2) {
      _1(s$p.emit, x);
    });
    return s$p;
  }
  function tap(f2, s3) {
    var s$p = base2("tap", s3.value);
    _1(s3.sub, function(x) {
      _1(f2, x);
      _1(s$p.emit, x);
    });
    return s$p;
  }
  function pair(s1, s22) {
    var subs = {
      contents: 0
    };
    var s$p = {};
    var emit2 = function(x) {
      s$p.value = x;
      dispatch(x, subs);
    };
    var sub6 = function(k) {
      add2(k, subs);
    };
    update_dummy(s$p, {
      name: "pair",
      value: [
        s1.value,
        s22.value
      ],
      emit: emit2,
      sub: sub6
    });
    _1(s1.sub, function(x1) {
      emit2([
        x1,
        s22.value
      ]);
    });
    _1(s22.sub, function(x2) {
      emit2([
        s1.value,
        x2
      ]);
    });
    return s$p;
  }
  function triple(s1, s22, s3) {
    var subs = {
      contents: 0
    };
    var s$p = {};
    var emit2 = function(x) {
      s$p.value = x;
      dispatch(x, subs);
    };
    var sub6 = function(k) {
      add2(k, subs);
    };
    update_dummy(s$p, {
      name: "triple",
      value: [
        s1.value,
        s22.value,
        s3.value
      ],
      emit: emit2,
      sub: sub6
    });
    _1(s1.sub, function(x1) {
      emit2([
        x1,
        s22.value,
        s3.value
      ]);
    });
    _1(s22.sub, function(x2) {
      emit2([
        s1.value,
        x2,
        s3.value
      ]);
    });
    _1(s3.sub, function(x3) {
      emit2([
        s1.value,
        s22.value,
        x3
      ]);
    });
    return s$p;
  }
  function filter(pred, seed, s3) {
    var subs = {
      contents: 0
    };
    var sub6 = function(k) {
      add2(k, subs);
    };
    var s$p = {};
    var emit2 = function(x) {
      if (_1(pred, x)) {
        s$p.value = x;
        return dispatch(s$p.value, subs);
      }
    };
    update_dummy(s$p, {
      name: "filter",
      value: _1(pred, s3.value) ? s3.value : seed,
      emit: emit2,
      sub: sub6
    });
    _1(s3.sub, emit2);
    return s$p;
  }
  function filter_map(f2, seed, s3) {
    var subs = {
      contents: 0
    };
    var sub6 = function(k) {
      add2(k, subs);
    };
    var s$p = {};
    var emit2 = function(x2) {
      s$p.value = x2;
      dispatch(s$p.value, subs);
    };
    var x = _1(f2, s3.value);
    update_dummy(s$p, {
      name: "filter_map",
      value: x !== void 0 ? valFromOption(x) : seed,
      emit: emit2,
      sub: sub6
    });
    _1(s3.sub, function(x2) {
      var x$p = _1(f2, x2);
      if (x$p !== void 0) {
        return emit2(valFromOption(x$p));
      }
    });
    return s$p;
  }
  function reduce(f2, init2, s3) {
    var subs = {
      contents: 0
    };
    var sub6 = function(k) {
      add2(k, subs);
    };
    var s$p = {};
    var emit2 = function(x) {
      s$p.value = x;
      dispatch(x, subs);
    };
    update_dummy(s$p, {
      name: "reduce",
      value: _2(f2, init2, s3.value),
      emit: emit2,
      sub: sub6
    });
    _1(s3.sub, function(value3) {
      _1(s$p.emit, _2(f2, s$p.value, value3));
    });
    return s$p;
  }
  function reducer(f2, init2) {
    var events = base2("reducer", void 0);
    var state_signal = reduce(function(state2, event_opt) {
      if (event_opt !== void 0) {
        return _2(f2, state2, valFromOption(event_opt));
      } else {
        return state2;
      }
    }, init2, events);
    var dispatch2 = function($$event) {
      _1(events.emit, some($$event));
    };
    return [
      state_signal,
      dispatch2
    ];
  }
  function select2(l) {
    if (!l) {
      return invalid_arg("Signal.select: empty signal list");
    }
    var subs = {
      contents: 0
    };
    var sub6 = function(k) {
      add2(k, subs);
    };
    var s$p = {};
    var emit2 = function(x) {
      s$p.value = x;
      dispatch(x, subs);
    };
    update_dummy(s$p, {
      name: "select",
      value: l.hd.value,
      emit: emit2,
      sub: sub6
    });
    iter(function(s3) {
      _1(s3.sub, emit2);
    }, l);
    return s$p;
  }
  function uniq(equalOpt, s3) {
    var equal4 = equalOpt !== void 0 ? equalOpt : function(prim0, prim1) {
      return prim0 === prim1;
    };
    var subs = {
      contents: 0
    };
    var s$p = {};
    var emit2 = function(x) {
      if (!_2(equal4, x, s$p.value)) {
        s$p.value = x;
        return dispatch(s$p.value, subs);
      }
    };
    var sub6 = function(k) {
      add2(k, subs);
    };
    update_dummy(s$p, {
      name: "uniq",
      value: s3.value,
      emit: emit2,
      sub: sub6
    });
    _1(s3.sub, emit2);
    return s$p;
  }
  function map22(f2, s1, s22) {
    var subs = {
      contents: 0
    };
    var s$p = {};
    var emit2 = function(x) {
      s$p.value = x;
      dispatch(x, subs);
    };
    var sub6 = function(k) {
      add2(k, subs);
    };
    update_dummy(s$p, {
      name: "map2",
      value: _2(f2, s1.value, s22.value),
      emit: emit2,
      sub: sub6
    });
    _1(s1.sub, function(x1) {
      emit2(_2(f2, x1, s22.value));
    });
    _1(s22.sub, function(x2) {
      emit2(_2(f2, s1.value, x2));
    });
    return s$p;
  }
  function map32(f2, s1, s22, s3) {
    var subs = {
      contents: 0
    };
    var s$p = {};
    var emit2 = function(x) {
      s$p.value = x;
      dispatch(x, subs);
    };
    var sub6 = function(k) {
      add2(k, subs);
    };
    update_dummy(s$p, {
      name: "map3",
      value: _3(f2, s1.value, s22.value, s3.value),
      emit: emit2,
      sub: sub6
    });
    _1(s1.sub, function(x1) {
      emit2(_3(f2, x1, s22.value, s3.value));
    });
    _1(s22.sub, function(x2) {
      emit2(_3(f2, s1.value, x2, s3.value));
    });
    _1(s3.sub, function(x3) {
      emit2(_3(f2, s1.value, s22.value, x3));
    });
    return s$p;
  }
  function sample(s1, s22) {
    var s$p = base2("sample", s22.value);
    _1(s1.sub, function(param2) {
      _1(s$p.emit, s22.value);
    });
    return s$p;
  }
  function apply(f_s, x_s) {
    var subs = {
      contents: 0
    };
    var s$p = {};
    var emit2 = function(x) {
      s$p.value = x;
      dispatch(x, subs);
    };
    var sub6 = function(k) {
      add2(k, subs);
    };
    update_dummy(s$p, {
      name: "apply",
      value: _1(f_s.value, x_s.value),
      emit: emit2,
      sub: sub6
    });
    _1(f_s.sub, function(f2) {
      emit2(_1(f2, x_s.value));
    });
    _1(x_s.sub, function(x) {
      emit2(_1(f_s.value, x));
    });
    return s$p;
  }
  function never_equal(param2, param$1) {
    return false;
  }
  function emitter(equalOpt, init2, emitter$1) {
    var equal4 = equalOpt !== void 0 ? equalOpt : never_equal;
    var subs = {
      contents: 0
    };
    var sub6 = function(k) {
      add2(k, subs);
    };
    var s3 = {};
    var emit2 = function(x) {
      if (!_2(equal4, x, s3.value)) {
        s3.value = x;
        return dispatch(s3.value, subs);
      }
    };
    update_dummy(s3, {
      name: "emitter",
      value: init2,
      emit: emit2,
      sub: sub6
    });
    _1(emitter$1, emit2);
    return s3;
  }
  var never = s2;

  // node_modules/rescript/lib/es6/array.js
  function blit2(a1, ofs1, a2, ofs2, len) {
    if (len < 0 || ofs1 < 0 || ofs1 > (a1.length - len | 0) || ofs2 < 0 || ofs2 > (a2.length - len | 0)) {
      throw {
        RE_EXN_ID: "Invalid_argument",
        _1: "Array.blit",
        Error: new Error()
      };
    }
    blit(a1, ofs1, a2, ofs2, len);
  }
  function list_length(_accu, _param) {
    while (true) {
      var param2 = _param;
      var accu = _accu;
      if (!param2) {
        return accu;
      }
      _param = param2.tl;
      _accu = accu + 1 | 0;
      continue;
    }
    ;
  }
  function of_list(l) {
    if (!l) {
      return [];
    }
    var a2 = make(list_length(0, l), l.hd);
    var _i = 1;
    var _param = l.tl;
    while (true) {
      var param2 = _param;
      var i4 = _i;
      if (!param2) {
        return a2;
      }
      a2[i4] = param2.hd;
      _param = param2.tl;
      _i = i4 + 1 | 0;
      continue;
    }
    ;
  }

  // node_modules/rescript/lib/es6/int32.js
  var max_int2 = 2147483647;

  // node_modules/rescript/lib/es6/int64.js
  var max_int3 = max_int;

  // node_modules/rescript/lib/es6/caml_md5.js
  function cmn(q2, a2, b2, x, s3, t2) {
    var a$1 = ((a2 + q2 | 0) + x | 0) + t2 | 0;
    return (a$1 << s3 | a$1 >>> (32 - s3 | 0) | 0) + b2 | 0;
  }
  function f(a2, b2, c, d, x, s3, t2) {
    return cmn(b2 & c | (b2 ^ -1) & d, a2, b2, x, s3, t2);
  }
  function g(a2, b2, c, d, x, s3, t2) {
    return cmn(b2 & d | c & (d ^ -1), a2, b2, x, s3, t2);
  }
  function h(a2, b2, c, d, x, s3, t2) {
    return cmn(b2 ^ c ^ d, a2, b2, x, s3, t2);
  }
  function i2(a2, b2, c, d, x, s3, t2) {
    return cmn(c ^ (b2 | d ^ -1), a2, b2, x, s3, t2);
  }
  function cycle(x, k) {
    var a2 = x[0];
    var b2 = x[1];
    var c = x[2];
    var d = x[3];
    a2 = f(a2, b2, c, d, k[0], 7, -680876936);
    d = f(d, a2, b2, c, k[1], 12, -389564586);
    c = f(c, d, a2, b2, k[2], 17, 606105819);
    b2 = f(b2, c, d, a2, k[3], 22, -1044525330);
    a2 = f(a2, b2, c, d, k[4], 7, -176418897);
    d = f(d, a2, b2, c, k[5], 12, 1200080426);
    c = f(c, d, a2, b2, k[6], 17, -1473231341);
    b2 = f(b2, c, d, a2, k[7], 22, -45705983);
    a2 = f(a2, b2, c, d, k[8], 7, 1770035416);
    d = f(d, a2, b2, c, k[9], 12, -1958414417);
    c = f(c, d, a2, b2, k[10], 17, -42063);
    b2 = f(b2, c, d, a2, k[11], 22, -1990404162);
    a2 = f(a2, b2, c, d, k[12], 7, 1804603682);
    d = f(d, a2, b2, c, k[13], 12, -40341101);
    c = f(c, d, a2, b2, k[14], 17, -1502002290);
    b2 = f(b2, c, d, a2, k[15], 22, 1236535329);
    a2 = g(a2, b2, c, d, k[1], 5, -165796510);
    d = g(d, a2, b2, c, k[6], 9, -1069501632);
    c = g(c, d, a2, b2, k[11], 14, 643717713);
    b2 = g(b2, c, d, a2, k[0], 20, -373897302);
    a2 = g(a2, b2, c, d, k[5], 5, -701558691);
    d = g(d, a2, b2, c, k[10], 9, 38016083);
    c = g(c, d, a2, b2, k[15], 14, -660478335);
    b2 = g(b2, c, d, a2, k[4], 20, -405537848);
    a2 = g(a2, b2, c, d, k[9], 5, 568446438);
    d = g(d, a2, b2, c, k[14], 9, -1019803690);
    c = g(c, d, a2, b2, k[3], 14, -187363961);
    b2 = g(b2, c, d, a2, k[8], 20, 1163531501);
    a2 = g(a2, b2, c, d, k[13], 5, -1444681467);
    d = g(d, a2, b2, c, k[2], 9, -51403784);
    c = g(c, d, a2, b2, k[7], 14, 1735328473);
    b2 = g(b2, c, d, a2, k[12], 20, -1926607734);
    a2 = h(a2, b2, c, d, k[5], 4, -378558);
    d = h(d, a2, b2, c, k[8], 11, -2022574463);
    c = h(c, d, a2, b2, k[11], 16, 1839030562);
    b2 = h(b2, c, d, a2, k[14], 23, -35309556);
    a2 = h(a2, b2, c, d, k[1], 4, -1530992060);
    d = h(d, a2, b2, c, k[4], 11, 1272893353);
    c = h(c, d, a2, b2, k[7], 16, -155497632);
    b2 = h(b2, c, d, a2, k[10], 23, -1094730640);
    a2 = h(a2, b2, c, d, k[13], 4, 681279174);
    d = h(d, a2, b2, c, k[0], 11, -358537222);
    c = h(c, d, a2, b2, k[3], 16, -722521979);
    b2 = h(b2, c, d, a2, k[6], 23, 76029189);
    a2 = h(a2, b2, c, d, k[9], 4, -640364487);
    d = h(d, a2, b2, c, k[12], 11, -421815835);
    c = h(c, d, a2, b2, k[15], 16, 530742520);
    b2 = h(b2, c, d, a2, k[2], 23, -995338651);
    a2 = i2(a2, b2, c, d, k[0], 6, -198630844);
    d = i2(d, a2, b2, c, k[7], 10, 1126891415);
    c = i2(c, d, a2, b2, k[14], 15, -1416354905);
    b2 = i2(b2, c, d, a2, k[5], 21, -57434055);
    a2 = i2(a2, b2, c, d, k[12], 6, 1700485571);
    d = i2(d, a2, b2, c, k[3], 10, -1894986606);
    c = i2(c, d, a2, b2, k[10], 15, -1051523);
    b2 = i2(b2, c, d, a2, k[1], 21, -2054922799);
    a2 = i2(a2, b2, c, d, k[8], 6, 1873313359);
    d = i2(d, a2, b2, c, k[15], 10, -30611744);
    c = i2(c, d, a2, b2, k[6], 15, -1560198380);
    b2 = i2(b2, c, d, a2, k[13], 21, 1309151649);
    a2 = i2(a2, b2, c, d, k[4], 6, -145523070);
    d = i2(d, a2, b2, c, k[11], 10, -1120210379);
    c = i2(c, d, a2, b2, k[2], 15, 718787259);
    b2 = i2(b2, c, d, a2, k[9], 21, -343485551);
    x[0] = a2 + x[0] | 0;
    x[1] = b2 + x[1] | 0;
    x[2] = c + x[2] | 0;
    x[3] = d + x[3] | 0;
  }
  var state = [
    1732584193,
    -271733879,
    -1732584194,
    271733878
  ];
  var md5blk = [
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0
  ];
  function md5_string(s3, start, len) {
    var s$1 = s3.slice(start, len);
    var n = s$1.length;
    state[0] = 1732584193;
    state[1] = -271733879;
    state[2] = -1732584194;
    state[3] = 271733878;
    for (var i4 = 0; i4 <= 15; ++i4) {
      md5blk[i4] = 0;
    }
    var i_end = n / 64 | 0;
    for (var i$12 = 1; i$12 <= i_end; ++i$12) {
      for (var j = 0; j <= 15; ++j) {
        var k = ((i$12 << 6) - 64 | 0) + (j << 2) | 0;
        md5blk[j] = ((s$1.charCodeAt(k) + (s$1.charCodeAt(k + 1 | 0) << 8) | 0) + (s$1.charCodeAt(k + 2 | 0) << 16) | 0) + (s$1.charCodeAt(k + 3 | 0) << 24) | 0;
      }
      cycle(state, md5blk);
    }
    var s_tail = s$1.slice(i_end << 6);
    for (var kk = 0; kk <= 15; ++kk) {
      md5blk[kk] = 0;
    }
    var i_end$1 = s_tail.length - 1 | 0;
    for (var i$22 = 0; i$22 <= i_end$1; ++i$22) {
      md5blk[i$22 / 4 | 0] = md5blk[i$22 / 4 | 0] | s_tail.charCodeAt(i$22) << (i$22 % 4 << 3);
    }
    var i$3 = i_end$1 + 1 | 0;
    md5blk[i$3 / 4 | 0] = md5blk[i$3 / 4 | 0] | 128 << (i$3 % 4 << 3);
    if (i$3 > 55) {
      cycle(state, md5blk);
      for (var i$4 = 0; i$4 <= 15; ++i$4) {
        md5blk[i$4] = 0;
      }
    }
    md5blk[14] = n << 3;
    cycle(state, md5blk);
    return String.fromCharCode(state[0] & 255, state[0] >> 8 & 255, state[0] >> 16 & 255, state[0] >> 24 & 255, state[1] & 255, state[1] >> 8 & 255, state[1] >> 16 & 255, state[1] >> 24 & 255, state[2] & 255, state[2] >> 8 & 255, state[2] >> 16 & 255, state[2] >> 24 & 255, state[3] & 255, state[3] >> 8 & 255, state[3] >> 16 & 255, state[3] >> 24 & 255);
  }

  // node_modules/rescript/lib/es6/digest.js
  function string2(str) {
    return md5_string(str, 0, str.length);
  }

  // node_modules/rescript/lib/es6/random.js
  function random_seed(param2) {
    return [Math.floor(Math.random() * 2147483647)];
  }
  function assign(st1, st2) {
    blit2(st2.st, 0, st1.st, 0, 55);
    st1.idx = st2.idx;
  }
  function full_init(s3, seed) {
    var combine = function(accu2, x) {
      return string2(accu2 + String(x));
    };
    var extract = function(d) {
      return ((get2(d, 0) + (get2(d, 1) << 8) | 0) + (get2(d, 2) << 16) | 0) + (get2(d, 3) << 24) | 0;
    };
    var seed$1 = seed.length === 0 ? [0] : seed;
    var l = seed$1.length;
    for (var i4 = 0; i4 <= 54; ++i4) {
      set(s3.st, i4, i4);
    }
    var accu = "x";
    for (var i$12 = 0, i_finish = 54 + (55 > l ? 55 : l) | 0; i$12 <= i_finish; ++i$12) {
      var j = i$12 % 55;
      var k = i$12 % l;
      accu = combine(accu, get(seed$1, k));
      set(s3.st, j, (get(s3.st, j) ^ extract(accu)) & 1073741823);
    }
    s3.idx = 0;
  }
  function make5(seed) {
    var result = {
      st: make(55, 0),
      idx: 0
    };
    full_init(result, seed);
    return result;
  }
  function make_self_init(param2) {
    return make5(random_seed(void 0));
  }
  function copy(s3) {
    var result = {
      st: make(55, 0),
      idx: 0
    };
    assign(result, s3);
    return result;
  }
  function bits(s3) {
    s3.idx = (s3.idx + 1 | 0) % 55;
    var curval = get(s3.st, s3.idx);
    var newval = get(s3.st, (s3.idx + 24 | 0) % 55) + (curval ^ curval >>> 25 & 31) | 0;
    var newval30 = newval & 1073741823;
    set(s3.st, s3.idx, newval30);
    return newval30;
  }
  function $$int2(s3, bound) {
    if (bound > 1073741823 || bound <= 0) {
      throw {
        RE_EXN_ID: "Invalid_argument",
        _1: "Random.int",
        Error: new Error()
      };
    }
    while (true) {
      var r = bits(s3);
      var v = r % bound;
      if ((r - v | 0) <= ((1073741823 - bound | 0) + 1 | 0)) {
        return v;
      }
      continue;
    }
    ;
  }
  function int32(s3, bound) {
    if (bound <= 0) {
      throw {
        RE_EXN_ID: "Invalid_argument",
        _1: "Random.int32",
        Error: new Error()
      };
    }
    while (true) {
      var b1 = bits(s3);
      var b2 = (bits(s3) & 1) << 30;
      var r = b1 | b2;
      var v = r % bound;
      if ((r - v | 0) <= ((max_int2 - bound | 0) + 1 | 0)) {
        return v;
      }
      continue;
    }
    ;
  }
  function int64(s3, bound) {
    if (i64_le(bound, zero)) {
      throw {
        RE_EXN_ID: "Invalid_argument",
        _1: "Random.int64",
        Error: new Error()
      };
    }
    while (true) {
      var b1 = of_int32(bits(s3));
      var b2 = lsl_(of_int32(bits(s3)), 30);
      var b3 = lsl_(of_int32(bits(s3) & 7), 60);
      var r = or_(b1, or_(b2, b3));
      var v = mod_(r, bound);
      if (!i64_gt(sub2(r, v), add(sub2(max_int3, bound), one))) {
        return v;
      }
      continue;
    }
    ;
  }
  function rawfloat(s3) {
    var r1 = bits(s3);
    var r2 = bits(s3);
    return (r1 / 1073741824 + r2) / 1073741824;
  }
  function $$float(s3, bound) {
    return rawfloat(s3) * bound;
  }
  function bool2(s3) {
    return (bits(s3) & 1) === 0;
  }
  var State = {
    make: make5,
    make_self_init,
    copy,
    bits,
    $$int: $$int2,
    int32,
    int64,
    $$float,
    bool: bool2
  };

  // node_modules/rescript/lib/es6/caml_hash_primitive.js
  function rotl32(x, n) {
    return x << n | x >>> (32 - n | 0) | 0;
  }
  function hash_mix_int(h7, d) {
    var d$1 = d;
    d$1 = Math.imul(d$1, -862048943);
    d$1 = rotl32(d$1, 15);
    d$1 = Math.imul(d$1, 461845907);
    var h$1 = h7 ^ d$1;
    h$1 = rotl32(h$1, 13);
    return (h$1 + (h$1 << 2) | 0) - 430675100 | 0;
  }
  function hash_final_mix(h7) {
    var h$1 = h7 ^ h7 >>> 16;
    h$1 = Math.imul(h$1, -2048144789);
    h$1 = h$1 ^ h$1 >>> 13;
    h$1 = Math.imul(h$1, -1028477387);
    return h$1 ^ h$1 >>> 16;
  }
  function hash_mix_string(h7, s3) {
    var len = s3.length;
    var block = (len / 4 | 0) - 1 | 0;
    var hash3 = h7;
    for (var i4 = 0; i4 <= block; ++i4) {
      var j = i4 << 2;
      var w = s3.charCodeAt(j) | s3.charCodeAt(j + 1 | 0) << 8 | s3.charCodeAt(j + 2 | 0) << 16 | s3.charCodeAt(j + 3 | 0) << 24;
      hash3 = hash_mix_int(hash3, w);
    }
    var modulo = len & 3;
    if (modulo !== 0) {
      var w$1 = modulo === 3 ? s3.charCodeAt(len - 1 | 0) << 16 | s3.charCodeAt(len - 2 | 0) << 8 | s3.charCodeAt(len - 3 | 0) : modulo === 2 ? s3.charCodeAt(len - 1 | 0) << 8 | s3.charCodeAt(len - 2 | 0) : s3.charCodeAt(len - 1 | 0);
      hash3 = hash_mix_int(hash3, w$1);
    }
    hash3 = hash3 ^ len;
    return hash3;
  }

  // node_modules/rescript/lib/es6/caml_hash.js
  function push_back(q2, v) {
    var cell = {
      content: v,
      next: void 0
    };
    var last = q2.last;
    if (last !== void 0) {
      q2.length = q2.length + 1 | 0;
      last.next = cell;
      q2.last = cell;
    } else {
      q2.length = 1;
      q2.first = cell;
      q2.last = cell;
    }
  }
  function unsafe_pop(q2) {
    var cell = q2.first;
    var next = cell.next;
    if (next === void 0) {
      q2.length = 0;
      q2.first = void 0;
      q2.last = void 0;
    } else {
      q2.length = q2.length - 1 | 0;
      q2.first = next;
    }
    return cell.content;
  }
  function hash(count, _limit, seed, obj2) {
    var s3 = seed;
    if (typeof obj2 === "number") {
      var u2 = obj2 | 0;
      s3 = hash_mix_int(s3, (u2 + u2 | 0) + 1 | 0);
      return hash_final_mix(s3);
    }
    if (typeof obj2 === "string") {
      s3 = hash_mix_string(s3, obj2);
      return hash_final_mix(s3);
    }
    var queue = {
      length: 0,
      first: void 0,
      last: void 0
    };
    var num = count;
    push_back(queue, obj2);
    num = num - 1 | 0;
    while (queue.length !== 0 && num > 0) {
      var obj$1 = unsafe_pop(queue);
      if (typeof obj$1 === "number") {
        var u$1 = obj$1 | 0;
        s3 = hash_mix_int(s3, (u$1 + u$1 | 0) + 1 | 0);
        num = num - 1 | 0;
      } else if (typeof obj$1 === "string") {
        s3 = hash_mix_string(s3, obj$1);
        num = num - 1 | 0;
      } else if (typeof obj$1 !== "boolean" && typeof obj$1 !== "undefined" && typeof obj$1 !== "symbol" && typeof obj$1 !== "function") {
        var size = obj$1.length | 0;
        if (size !== 0) {
          var obj_tag = obj$1.TAG | 0;
          var tag = size << 10 | obj_tag;
          if (obj_tag === 248) {
            s3 = hash_mix_int(s3, obj$1[1]);
          } else {
            s3 = hash_mix_int(s3, tag);
            var v = size - 1 | 0;
            var block = v < num ? v : num;
            for (var i4 = 0; i4 <= block; ++i4) {
              push_back(queue, obj$1[i4]);
            }
          }
        } else {
          var size$1 = function(obj3, cb) {
            var size2 = 0;
            for (var k in obj3) {
              cb(obj3[k]);
              ++size2;
            }
            return size2;
          }(obj$1, function(v2) {
            push_back(queue, v2);
          });
          s3 = hash_mix_int(s3, size$1 << 10 | 0);
        }
      }
    }
    ;
    return hash_final_mix(s3);
  }

  // node_modules/rescript/lib/es6/camlinternalLazy.js
  var Undefined = /* @__PURE__ */ create("CamlinternalLazy.Undefined");
  function forward_with_closure(blk, closure) {
    var result = closure();
    blk.VAL = result;
    blk.LAZY_DONE = true;
    return result;
  }
  function raise_undefined() {
    throw {
      RE_EXN_ID: Undefined,
      Error: new Error()
    };
  }
  function force(lzv) {
    if (lzv.LAZY_DONE) {
      return lzv.VAL;
    } else {
      var closure = lzv.VAL;
      lzv.VAL = raise_undefined;
      try {
        return forward_with_closure(lzv, closure);
      } catch (e) {
        lzv.VAL = function() {
          throw e;
        };
        throw e;
      }
    }
  }

  // node_modules/rescript/lib/es6/hashtbl.js
  function hash2(x) {
    return hash(10, 100, 0, x);
  }
  function flip_ongoing_traversal(h7) {
    h7.initial_size = -h7.initial_size | 0;
  }
  var randomized = {
    contents: false
  };
  var prng = {
    LAZY_DONE: false,
    VAL: function() {
      return State.make_self_init(void 0);
    }
  };
  function power_2_above(_x, n) {
    while (true) {
      var x = _x;
      if (x >= n) {
        return x;
      }
      if (x << 1 < x) {
        return x;
      }
      _x = x << 1;
      continue;
    }
    ;
  }
  function create3(randomOpt, initial_size) {
    var random = randomOpt !== void 0 ? randomOpt : randomized.contents;
    var s3 = power_2_above(16, initial_size);
    var seed = random ? State.bits(force(prng)) : 0;
    return {
      size: 0,
      data: make(s3, 0),
      seed,
      initial_size: s3
    };
  }
  function resize(indexfun, h7) {
    var odata = h7.data;
    var osize = odata.length;
    var nsize = osize << 1;
    if (nsize < osize) {
      return;
    }
    var ndata = make(nsize, 0);
    var ndata_tail = make(nsize, 0);
    var inplace = h7.initial_size >= 0;
    h7.data = ndata;
    var insert_bucket = function(_cell) {
      while (true) {
        var cell = _cell;
        if (!cell) {
          return;
        }
        var key2 = cell.key;
        var data2 = cell.data;
        var next = cell.next;
        var cell$1 = inplace ? cell : {
          key: key2,
          data: data2,
          next: 0
        };
        var nidx = _2(indexfun, h7, key2);
        var tail2 = get(ndata_tail, nidx);
        if (tail2) {
          tail2.next = cell$1;
        } else {
          set(ndata, nidx, cell$1);
        }
        set(ndata_tail, nidx, cell$1);
        _cell = next;
        continue;
      }
      ;
    };
    for (var i4 = 0; i4 < osize; ++i4) {
      insert_bucket(get(odata, i4));
    }
    if (!inplace) {
      return;
    }
    for (var i$12 = 0; i$12 < nsize; ++i$12) {
      var tail = get(ndata_tail, i$12);
      if (tail) {
        tail.next = 0;
      }
    }
  }
  function key_index(h7, key2) {
    return hash(10, 100, h7.seed, key2) & (h7.data.length - 1 | 0);
  }
  function add3(h7, key2, data2) {
    var i4 = key_index(h7, key2);
    var bucket = {
      key: key2,
      data: data2,
      next: get(h7.data, i4)
    };
    set(h7.data, i4, bucket);
    h7.size = h7.size + 1 | 0;
    if (h7.size > h7.data.length << 1) {
      return resize(key_index, h7);
    }
  }
  function remove(h7, key2) {
    var i4 = key_index(h7, key2);
    var _prec = 0;
    var _c = get(h7.data, i4);
    while (true) {
      var c = _c;
      var prec = _prec;
      if (!c) {
        return;
      }
      var k = c.key;
      var next = c.next;
      if (equal(k, key2)) {
        h7.size = h7.size - 1 | 0;
        if (prec) {
          prec.next = next;
          return;
        } else {
          return set(h7.data, i4, next);
        }
      }
      _c = next;
      _prec = c;
      continue;
    }
    ;
  }
  function find_opt(h7, key2) {
    var match = get(h7.data, key_index(h7, key2));
    if (!match) {
      return;
    }
    var k1 = match.key;
    var d1 = match.data;
    var next1 = match.next;
    if (equal(key2, k1)) {
      return some(d1);
    }
    if (!next1) {
      return;
    }
    var k2 = next1.key;
    var d2 = next1.data;
    var next2 = next1.next;
    if (equal(key2, k2)) {
      return some(d2);
    }
    if (!next2) {
      return;
    }
    var k3 = next2.key;
    var d3 = next2.data;
    var next3 = next2.next;
    if (equal(key2, k3)) {
      return some(d3);
    } else {
      var _param = next3;
      while (true) {
        var param2 = _param;
        if (!param2) {
          return;
        }
        var k = param2.key;
        var data2 = param2.data;
        var next = param2.next;
        if (equal(key2, k)) {
          return some(data2);
        }
        _param = next;
        continue;
      }
      ;
    }
  }
  function replace_bucket(key2, data2, _slot) {
    while (true) {
      var slot = _slot;
      if (!slot) {
        return true;
      }
      var k = slot.key;
      var next = slot.next;
      if (equal(k, key2)) {
        slot.key = key2;
        slot.data = data2;
        return false;
      }
      _slot = next;
      continue;
    }
    ;
  }
  function replace(h7, key2, data2) {
    var i4 = key_index(h7, key2);
    var l = get(h7.data, i4);
    if (replace_bucket(key2, data2, l)) {
      set(h7.data, i4, {
        key: key2,
        data: data2,
        next: l
      });
      h7.size = h7.size + 1 | 0;
      if (h7.size > h7.data.length << 1) {
        return resize(key_index, h7);
      } else {
        return;
      }
    }
  }
  function iter3(f2, h7) {
    var do_bucket = function(_param) {
      while (true) {
        var param2 = _param;
        if (!param2) {
          return;
        }
        var key2 = param2.key;
        var data2 = param2.data;
        var next = param2.next;
        _2(f2, key2, data2);
        _param = next;
        continue;
      }
      ;
    };
    var old_trav = h7.initial_size < 0;
    if (!old_trav) {
      flip_ongoing_traversal(h7);
    }
    try {
      var d = h7.data;
      for (var i4 = 0, i_finish = d.length; i4 < i_finish; ++i4) {
        do_bucket(get(d, i4));
      }
      if (!old_trav) {
        return flip_ongoing_traversal(h7);
      } else {
        return;
      }
    } catch (exn) {
      if (old_trav) {
        throw exn;
      }
      flip_ongoing_traversal(h7);
      throw exn;
    }
  }

  // lib/es6/src/helix/View.bs.js
  var i3 = {
    contents: -1
  };
  function gen_show_id(param2) {
    i3.contents = i3.contents + 1 | 0;
    return "show:" + String(i3.contents);
  }
  function insert_after_anchor(parent, anchor, node) {
    var anchor_sibling = _1(Dom.$$Node.next_sibling, anchor);
    if (anchor_sibling !== void 0) {
      return _3(Dom.$$Node.insert_before, parent, valFromOption(anchor_sibling), node);
    } else {
      return _2(Dom.$$Node.append_child, parent, node);
    }
  }
  function show(f2, s3) {
    var anchor = _1(Dom.$$Comment.as_node, _1(Dom.$$Comment.make, gen_show_id(void 0)));
    var fragment2 = _1(Dom.Document_fragment.as_node, _1(Dom.Document_fragment.make, void 0));
    _2(Dom.$$Node.append_child, fragment2, anchor);
    var prev_nodes = {
      contents: 0
    };
    var node0 = _1($$Node.Internal.of_html, _1(f2, get5(s3)));
    _2(Dom.$$Node.append_child, fragment2, node0);
    var is_fragment = $$instanceof2(_1(Dom.$$Node.as_js, node0), Dom.Document_fragment.t);
    if (is_fragment) {
      _2(Dom.$$Node.List.for_each, _1(Dom.$$Node.child_nodes, node0), function(child) {
        prev_nodes.contents = {
          hd: child,
          tl: prev_nodes.contents
        };
      });
    } else {
      prev_nodes.contents = {
        hd: node0,
        tl: prev_nodes.contents
      };
    }
    sub$1(function(x) {
      var parent = _1(Dom.$$Node.parent_node, anchor);
      var parent$1 = parent !== void 0 ? valFromOption(parent) : failwith("Html.show: cannot update unmounted element");
      var next = _1($$Node.Internal.of_html, _1(f2, x));
      Console.log([
        "prev nodes",
        of_list(prev_nodes.contents)
      ]);
      iter(function(child) {
        _2(Dom.$$Node.remove_child, parent$1, child);
      }, prev_nodes.contents);
      prev_nodes.contents = 0;
      var is_fragment2 = $$instanceof2(_1(Dom.$$Node.as_js, next), Dom.Document_fragment.t);
      if (is_fragment2) {
        _2(Dom.$$Node.List.for_each, _1(Dom.$$Node.child_nodes, next), function(child) {
          prev_nodes.contents = {
            hd: child,
            tl: prev_nodes.contents
          };
        });
      } else {
        prev_nodes.contents = {
          hd: next,
          tl: prev_nodes.contents
        };
      }
      insert_after_anchor(parent$1, anchor, next);
    }, s3);
    return _1($$Node.Internal.to_html, fragment2);
  }
  var i$1 = {
    contents: -1
  };
  function gen_conditional_id(param2) {
    i$1.contents = i$1.contents + 1 | 0;
    return "conditional:" + String(i$1.contents);
  }
  function conditional(condition_s, html2) {
    var anchor = _1(Dom.$$Comment.as_node, _1(Dom.$$Comment.make, gen_conditional_id(void 0)));
    var fragment2 = _1(Dom.Document_fragment.as_node, _1(Dom.Document_fragment.make, void 0));
    _2(Dom.$$Node.append_child, fragment2, anchor);
    var prev_ref = {
      contents: void 0
    };
    if (get5(condition_s)) {
      var node0 = _1($$Node.Internal.of_html, html2);
      _2(Dom.$$Node.append_child, fragment2, node0);
      prev_ref.contents = some(node0);
    }
    sub$1(function(condition) {
      var parent = _1(Dom.$$Node.parent_node, anchor);
      var parent$1 = parent !== void 0 ? valFromOption(parent) : failwith("Html.show: cannot update unmounted element");
      if (condition) {
        var next = _1($$Node.Internal.of_html, html2);
        var prev = prev_ref.contents;
        if (prev !== void 0) {
          _3(Dom.$$Node.replace_child, parent$1, valFromOption(prev), next);
          prev_ref.contents = some(next);
        } else {
          insert_after_anchor(parent$1, anchor, next);
          prev_ref.contents = some(next);
        }
        return;
      }
      var prev$1 = prev_ref.contents;
      if (prev$1 !== void 0) {
        _2(Dom.$$Node.remove_child, parent$1, valFromOption(prev$1));
        prev_ref.contents = void 0;
        return;
      }
    }, condition_s);
    return _1($$Node.Internal.to_html, fragment2);
  }
  var i$2 = {
    contents: -1
  };
  function gen_each_id(param2) {
    i$2.contents = i$2.contents + 1 | 0;
    return "each:" + String(i$2.contents);
  }
  function each(to_html2, items_s) {
    var anchor = _1(Dom.$$Comment.as_node, _1(Dom.$$Comment.make, gen_each_id(void 0)));
    var fragment2 = _1(Dom.Document_fragment.as_node, _1(Dom.Document_fragment.make, void 0));
    _2(Dom.$$Node.append_child, fragment2, anchor);
    var items0 = get5(items_s);
    var cache = create3(void 0, length(items0));
    iter(function(item0) {
      var hash3 = hash2(item0);
      var node = _1($$Node.Internal.of_html, _1(to_html2, item0));
      _2(Dom.$$Node.append_child, fragment2, node);
      add3(cache, hash3, [
        node,
        false
      ]);
    }, items0);
    sub$1(function(new_items) {
      iter(function(item) {
        var hash3 = hash2(item);
        var match = find_opt(cache, hash3);
        if (match !== void 0) {
          var cached_node = match[0];
          _2(Dom.$$Node.append_child, fragment2, cached_node);
          return replace(cache, hash3, [
            cached_node,
            true
          ]);
        }
        var new_node = _1($$Node.Internal.of_html, _1(to_html2, item));
        _2(Dom.$$Node.append_child, fragment2, new_node);
        add3(cache, hash3, [
          new_node,
          true
        ]);
      }, new_items);
      var parent = _1(Dom.$$Node.parent_node, anchor);
      var parent$1 = parent !== void 0 ? valFromOption(parent) : failwith("Html.each: cannot update unmounted elements");
      insert_after_anchor(parent$1, anchor, fragment2);
      iter3(function(hash3, param2) {
        var node = param2[0];
        if (param2[1]) {
          return replace(cache, hash3, [
            node,
            false
          ]);
        } else {
          _2(Dom.$$Node.remove_child, parent$1, node);
          return remove(cache, hash3);
        }
      }, cache);
    }, items_s);
    return _1($$Node.Internal.to_html, fragment2);
  }
  function bind2(attr_sig) {
    var prev0 = get5(attr_sig);
    var prev$p = {
      contents: _1($$Attr.Internal.of_attr, prev0)
    };
    var set5 = function(elem2) {
      _1(prev$p.contents.set, elem2);
      use(function(next) {
        var next$p = _1($$Attr.Internal.of_attr, next);
        _1(prev$p.contents.remove, elem2);
        _1(next$p.set, elem2);
        prev$p.contents = next$p;
      }, attr_sig);
    };
    var remove2 = function(elem2) {
      _1(prev$p.contents.remove, elem2);
    };
    return _1($$Attr.Internal.to_attr, {
      set: set5,
      remove: remove2
    });
  }
  function toggle(active_sig, attr0) {
    var active_sig$1 = uniq(function(prim0, prim1) {
      return prim0 === prim1;
    }, active_sig);
    var internal = _1($$Attr.Internal.of_attr, attr0);
    var should_activate0 = get5(active_sig$1);
    var set5 = function(elem2) {
      if (should_activate0) {
        _1(internal.set, elem2);
      }
      use(function(should_activate) {
        if (should_activate) {
          return _1(internal.set, elem2);
        } else {
          return _1(internal.remove, elem2);
        }
      }, active_sig$1);
    };
    return _1($$Attr.Internal.to_attr, {
      set: set5,
      remove: internal.remove
    });
  }
  function visible(cond) {
    return toggle(map3(function(prim) {
      return !prim;
    }, cond), style({
      hd: [
        "display",
        "none"
      ],
      tl: 0
    }));
  }

  // lib/es6/src/helix/Mouse.bs.js
  function position_of_mouse_event(m_ev) {
    return [
      _1(Dom.$$Event.Mouse.page_x, m_ev),
      _1(Dom.$$Event.Mouse.page_y, m_ev)
    ];
  }
  var position = emitter(void 0, [
    0,
    0
  ], function(emit2) {
    _3(Dom.$$Window.add_event_listener, Dom.$$Window.$$this, "mousemove", function(m_ev) {
      _1(emit2, position_of_mouse_event(m_ev));
    });
  });

  // lib/es6/src/helix/Helix.bs.js
  var Html$1 = Html_bs_exports;
  var Signal$1 = Signal_bs_exports;
  var View$1 = {
    show,
    each,
    conditional,
    bind: bind2,
    visible,
    toggle
  };

  // lib/es6/examples/demo-rescript/Demo.bs.js
  function app2(param2) {
    var what = _1(Signal$1.make, "a");
    return _2(Html$1.div, 0, {
      hd: _2(Html$1.div, {
        hd: _1(Html$1.style, {
          hd: [
            "gap",
            "10px"
          ],
          tl: {
            hd: [
              "display",
              "flex"
            ],
            tl: 0
          }
        }),
        tl: 0
      }, {
        hd: _2(Html$1.button, {
          hd: _2(Html$1.on, Dom.$$Event.click, function(param3) {
            _2(Signal$1.emit, "a", what);
          }),
          tl: 0
        }, {
          hd: _1(Html$1.text, "empty1"),
          tl: 0
        }),
        tl: {
          hd: _2(Html$1.button, {
            hd: _2(Html$1.on, Dom.$$Event.click, function(param3) {
              _2(Signal$1.emit, "b", what);
            }),
            tl: 0
          }, {
            hd: _1(Html$1.text, "empty2"),
            tl: 0
          }),
          tl: 0
        }
      }),
      tl: {
        hd: _1(Html$1.hr, 0),
        tl: {
          hd: View$1.show(function(what2) {
            if (what2 === "b") {
              return _1(Html$1.text, "B");
            } else {
              return _1(Html$1.text, "A");
            }
          }, what),
          tl: 0
        }
      }
    });
  }
  var root = _1(Dom.$$Document.get_element_by_id, "root");
  if (root !== void 0) {
    _2(Html$1.render, valFromOption(root), app2(void 0));
  } else {
    failwith("no #app");
  }
  var $$Event;
})();
