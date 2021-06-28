{ pkgs ? import <nixpkgs> {} }:

{
  "aff" = {
    name = "aff";
    version = "v6.0.0";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript-contrib/purescript-aff.git";
      rev = "v6.0.0";
      sha256 = "1780sgqyvbdgh8ynxmxn5d44vvhaz7kn9sv3l44c2s9q8xfjkfgm";
    };
  };
  "aff-promise" = {
    name = "aff-promise";
    version = "v3.0.0";
    src = pkgs.fetchgit {
      url = "https://github.com/nwolverson/purescript-aff-promise.git";
      rev = "v3.0.0";
      sha256 = "12fnlwcrj5p6kc5rls7qxwg53zd83gkdpklpmp8jyav945hlgbj2";
    };
  };
  "affjax" = {
    name = "affjax";
    version = "v12.0.0";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript-contrib/purescript-affjax.git";
      rev = "v12.0.0";
      sha256 = "1f2snaimnl9ry8f3kankfcyy50wkba54vvin4wsrglahqgs1nrgb";
    };
  };
  "ansi" = {
    name = "ansi";
    version = "v6.1.0";
    src = pkgs.fetchgit {
      url = "https://github.com/hdgarrood/purescript-ansi.git";
      rev = "v6.1.0";
      sha256 = "1jsll0h7nz13zgscs036cnkkc6frnlcnk6fwjdwsyp6wbmjri2zm";
    };
  };
  "argonaut" = {
    name = "argonaut";
    version = "v8.0.0";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript-contrib/purescript-argonaut.git";
      rev = "v8.0.0";
      sha256 = "05sq1102rl1phm2gadx0gp966yvk9q1r492bb30q1m0nz762q4v2";
    };
  };
  "argonaut-codecs" = {
    name = "argonaut-codecs";
    version = "v8.1.0";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript-contrib/purescript-argonaut-codecs.git";
      rev = "v8.1.0";
      sha256 = "11vmlq98s4jmg5grvdrrlfkqj9vk3la44ky8158a440ipcpinjkq";
    };
  };
  "argonaut-core" = {
    name = "argonaut-core";
    version = "v6.0.0";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript-contrib/purescript-argonaut-core.git";
      rev = "v6.0.0";
      sha256 = "13ka4xybc8ql54xlkkhy4919nnapfigdlk51ja85f8xwhr64x9kq";
    };
  };
  "argonaut-traversals" = {
    name = "argonaut-traversals";
    version = "v9.0.0";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript-contrib/purescript-argonaut-traversals.git";
      rev = "v9.0.0";
      sha256 = "0bj88s7rz50jfhyawq4h97lvbr3h7pksbqnz4lmh714f5fda6ncx";
    };
  };
  "arraybuffer-types" = {
    name = "arraybuffer-types";
    version = "v3.0.0";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript-contrib/purescript-arraybuffer-types.git";
      rev = "v3.0.0";
      sha256 = "19dh4k3n1lr8hbj15ivkiv1886s1q7brl9hic6zrkrr2rx1bz69r";
    };
  };
  "arrays" = {
    name = "arrays";
    version = "v6.0.1";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript/purescript-arrays.git";
      rev = "v6.0.1";
      sha256 = "0lm0m5hapimchzgfywr648pkw1hpggr6qibh8d19p2impbnc94c0";
    };
  };
  "avar" = {
    name = "avar";
    version = "v4.0.0";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript-contrib/purescript-avar.git";
      rev = "v4.0.0";
      sha256 = "005046wl61w6r5v3qwd16srhcx82vdz3yvp4xzad2xaasb6iq55l";
    };
  };
  "bifunctors" = {
    name = "bifunctors";
    version = "v5.0.0";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript/purescript-bifunctors.git";
      rev = "v5.0.0";
      sha256 = "0xc2hf8ccdgqw3m9qcmr38kmzv05fsxvakd07wyrqshvkzg3xn0d";
    };
  };
  "catenable-lists" = {
    name = "catenable-lists";
    version = "v6.0.1";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript/purescript-catenable-lists.git";
      rev = "v6.0.1";
      sha256 = "1lz06fx0za5sl65wccn5fl37mw3x4jnvrriz1gg0aqsmm9lag7ss";
    };
  };
  "colors" = {
    name = "colors";
    version = "v6.0.0";
    src = pkgs.fetchgit {
      url = "https://github.com/sharkdp/purescript-colors.git";
      rev = "v6.0.0";
      sha256 = "05gdjx8xhv7xxm1prrrc2brxjn1gi19qf1004syk8qx37slrjf87";
    };
  };
  "console" = {
    name = "console";
    version = "v5.0.0";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript/purescript-console.git";
      rev = "v5.0.0";
      sha256 = "0fzzzqjgrz33pb2jf7cdqpg09ilxb7bsrc7sbfq52wjg0sx9aq6g";
    };
  };
  "const" = {
    name = "const";
    version = "v5.0.0";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript/purescript-const.git";
      rev = "v5.0.0";
      sha256 = "0aq9qjbrvf8mf8hmas6imv4mg6n3zi13hkf449ns1hn12lw8qv4g";
    };
  };
  "contravariant" = {
    name = "contravariant";
    version = "v5.0.0";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript/purescript-contravariant.git";
      rev = "v5.0.0";
      sha256 = "029hb8i3n4759x4gc06wkfgr7wim5x1w5jy2bsiy42n0g731h5qc";
    };
  };
  "control" = {
    name = "control";
    version = "v5.0.0";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript/purescript-control.git";
      rev = "v5.0.0";
      sha256 = "06dc06yli4g5yr8fb9sdpqbhiaff37g977qcsbds9q2mlhnjgfx9";
    };
  };
  "css" = {
    name = "css";
    version = "v5.0.1";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript-contrib/purescript-css.git";
      rev = "v5.0.1";
      sha256 = "1ai4y35a6p4kckvwggb01zj0ykgxklbhl86kdzhymqky31s6ndcn";
    };
  };
  "datetime" = {
    name = "datetime";
    version = "v5.0.2";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript/purescript-datetime.git";
      rev = "v5.0.2";
      sha256 = "1mhzn2ymdkzki7wjlr9xrdbngm0886wmfbh2c46flnf9lmfyw54y";
    };
  };
  "debug" = {
    name = "debug";
    version = "v5.0.0";
    src = pkgs.fetchgit {
      url = "https://github.com/garyb/purescript-debug.git";
      rev = "v5.0.0";
      sha256 = "09j69bgrq8nzw1l3aj1hka3y5ycmcsn9dlgf22k5ifrd74iic60y";
    };
  };
  "distributive" = {
    name = "distributive";
    version = "v5.0.0";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript/purescript-distributive.git";
      rev = "v5.0.0";
      sha256 = "0788znmdyh6b1c9pln624ah397l88xmd3fxlxiy3z1qy8bzr4r54";
    };
  };
  "dom-filereader" = {
    name = "dom-filereader";
    version = "v5.0.0";
    src = pkgs.fetchgit {
      url = "https://github.com/nwolverson/purescript-dom-filereader";
      rev = "v5.0.0";
      sha256 = "0337r9qrnrgmfpssdvls5rz03y97kmxp2s8fr72i5hycih93yv74";
    };
  };
  "dom-simple" = {
    name = "dom-simple";
    version = "v0.2.7";
    src = pkgs.fetchgit {
      url = "https://github.com/irresponsible/purescript-dom-simple";
      rev = "v0.2.7";
      sha256 = "02f1vsjk5frva3p3xpbrnj2zg4rg01l07dnvc3i6axvc5k30i282";
    };
  };
  "effect" = {
    name = "effect";
    version = "v3.0.0";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript/purescript-effect.git";
      rev = "v3.0.0";
      sha256 = "1n9qr85knvpm4i0qhm8xbgfk46v9y843p76j278phfs9l6aywzsn";
    };
  };
  "either" = {
    name = "either";
    version = "v5.0.0";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript/purescript-either.git";
      rev = "v5.0.0";
      sha256 = "18dk159yyv7vs0xsnh9m5fajd7zy6zw5b2mpyd6nqdh3c6bb9wh6";
    };
  };
  "enums" = {
    name = "enums";
    version = "v5.0.0";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript/purescript-enums.git";
      rev = "v5.0.0";
      sha256 = "1lci5iy6s6cmh93bpkfcmp0j4n5dnij7dswb0075bk0kzd9xp7rs";
    };
  };
  "exceptions" = {
    name = "exceptions";
    version = "v5.0.0";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript/purescript-exceptions.git";
      rev = "v5.0.0";
      sha256 = "1yjbrx34a0rnxgpvywb63n9jzhkdgb2q2acyzbwh290mrrggc95x";
    };
  };
  "exists" = {
    name = "exists";
    version = "v5.0.0";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript/purescript-exists.git";
      rev = "v5.0.0";
      sha256 = "1kgq09c1pzwgyx9bmi2q0i5bsv2mj3v4sfy5bivhmj5k29h66lg7";
    };
  };
  "ffi-simple" = {
    name = "ffi-simple";
    version = "v0.2.10";
    src = pkgs.fetchgit {
      url = "https://github.com/irresponsible/purescript-ffi-simple";
      rev = "v0.2.10";
      sha256 = "14slcccmy96ml7r8rzhhqnw486qj1b385i9095fdymms78g5gnj5";
    };
  };
  "foldable-traversable" = {
    name = "foldable-traversable";
    version = "v5.0.1";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript/purescript-foldable-traversable.git";
      rev = "v5.0.1";
      sha256 = "182na4np7hk2dqyxywy4jij2csrzx4bz02m6bq8yx1j27hlgjvsd";
    };
  };
  "foreign" = {
    name = "foreign";
    version = "v6.0.1";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript/purescript-foreign.git";
      rev = "v6.0.1";
      sha256 = "16j7712cck79p8q53xbhn4hs886bm0ls5wvmchrhqnaghj48m85g";
    };
  };
  "foreign-generic" = {
    name = "foreign-generic";
    version = "v11.0.0";
    src = pkgs.fetchgit {
      url = "https://github.com/paf31/purescript-foreign-generic.git";
      rev = "v11.0.0";
      sha256 = "129bgngch2zi65838v2hcywx59gd3x56fq8zaasnwj5kwm34dxcw";
    };
  };
  "foreign-object" = {
    name = "foreign-object";
    version = "v3.0.0";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript/purescript-foreign-object.git";
      rev = "v3.0.0";
      sha256 = "0accw6qd93qqry19rskjgl7y54xi2wd70rglbqyjx6c5ybcjnavr";
    };
  };
  "fork" = {
    name = "fork";
    version = "v5.0.0";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript-contrib/purescript-fork.git";
      rev = "v5.0.0";
      sha256 = "1hyvaixza8151zbylk2kv859x48yyhla536lcjghcwd62vzfwmdn";
    };
  };
  "form-urlencoded" = {
    name = "form-urlencoded";
    version = "v6.0.1";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript-contrib/purescript-form-urlencoded.git";
      rev = "v6.0.1";
      sha256 = "0afg83r1hj68qkacf501m06nbhwl7bh5kzld6wdgvy0blhhh243r";
    };
  };
  "formula" = {
    name = "formula";
    version = "v0.2.1";
    src = pkgs.fetchgit {
      url = "https://github.com/poorscript/purescript-formula";
      rev = "v0.2.1";
      sha256 = "0511r2n51sz59ksalzvrcbwwhbpj42h2dj2zknns7dnhkdkmf4a6";
    };
  };
  "free" = {
    name = "free";
    version = "v6.0.1";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript/purescript-free.git";
      rev = "v6.0.1";
      sha256 = "0kpq83qjfjzf1l2f1cnnx36kjwnm5czgjyh2imwp3bna8js6sk39";
    };
  };
  "functions" = {
    name = "functions";
    version = "v5.0.0";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript/purescript-functions.git";
      rev = "v5.0.0";
      sha256 = "1gnk6xh5x04zcahn82gwp49qpglxd5jkfqn0i58m27jfihvblaxd";
    };
  };
  "functors" = {
    name = "functors";
    version = "v4.1.1";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript/purescript-functors.git";
      rev = "v4.1.1";
      sha256 = "0i1x14r54758s5jx5d7zy4l07mg6gabljadgybldnbpmdqk6b966";
    };
  };
  "gen" = {
    name = "gen";
    version = "v3.0.0";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript/purescript-gen.git";
      rev = "v3.0.0";
      sha256 = "1h396rqn1fc2c155i58vnaksqjrpajly128ah6wq1w426vwr1vrf";
    };
  };
  "globals" = {
    name = "globals";
    version = "v4.1.0";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript/purescript-globals";
      rev = "v4.1.0";
      sha256 = "03h4npdbsjr1mkjasdi071l0cl7zdf3l76a8f0x4ghx47yvpgadn";
    };
  };
  "http-methods" = {
    name = "http-methods";
    version = "v5.0.0";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript-contrib/purescript-http-methods.git";
      rev = "v5.0.0";
      sha256 = "1g0ywd5zpckmhq28mr14yr4k28hiii1px8r8xbdx8nv45ryw69l3";
    };
  };
  "identity" = {
    name = "identity";
    version = "v5.0.0";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript/purescript-identity.git";
      rev = "v5.0.0";
      sha256 = "0a58y71ihvb5b7plnn2sxsbphqzd9nzfafak4d5a576agn76q0ql";
    };
  };
  "integers" = {
    name = "integers";
    version = "v5.0.0";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript/purescript-integers.git";
      rev = "v5.0.0";
      sha256 = "1rrygw0ai61brnvgap7dfhdzacyhg5439pz6yrmmyg32cvf0znhv";
    };
  };
  "invariant" = {
    name = "invariant";
    version = "v5.0.0";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript/purescript-invariant.git";
      rev = "v5.0.0";
      sha256 = "0vwkbh7kv00g50xjgvxc0mv5b99mrj6q0sxznxwk32hb9hkbhy5l";
    };
  };
  "js-date" = {
    name = "js-date";
    version = "v7.0.0";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript-contrib/purescript-js-date.git";
      rev = "v7.0.0";
      sha256 = "1dpiwn65qww862ilpfbd06gwfazpxvz3jwvsjsdrcxqqfcbjp8n8";
    };
  };
  "js-timers" = {
    name = "js-timers";
    version = "v5.0.1";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript-contrib/purescript-js-timers.git";
      rev = "v5.0.1";
      sha256 = "0008paz0qkz5n1pfrzagkkac6jny9z2rd1ij10ww2k1pkb9cy59z";
    };
  };
  "js-uri" = {
    name = "js-uri";
    version = "v2.0.0";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript-contrib/purescript-js-uri.git";
      rev = "v2.0.0";
      sha256 = "1q34ir93cqbcl9g49vv1qfj8jxbbdj7f85a14y4mzd7yjq0a042g";
    };
  };
  "lazy" = {
    name = "lazy";
    version = "v5.0.0";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript/purescript-lazy.git";
      rev = "v5.0.0";
      sha256 = "1wxfx019911gbkifq266hgn67zwm89pxhi83bai77mva5n9j3f6l";
    };
  };
  "lcg" = {
    name = "lcg";
    version = "v3.0.0";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript/purescript-lcg.git";
      rev = "v3.0.0";
      sha256 = "04r9bmx9kc3jqx59hh9yqqkl95mf869la9as5h36jv85ynn464dx";
    };
  };
  "lists" = {
    name = "lists";
    version = "v6.0.1";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript/purescript-lists.git";
      rev = "v6.0.1";
      sha256 = "0xmg918s3mqvfvwgjfqcs1yvcz6hy2n7h3ygqz2iyvk868gz25qs";
    };
  };
  "markdown" = {
    name = "markdown";
    version = "2021-06-22";
    src = pkgs.fetchgit {
      url = "https://github.com/poorscript/purescript-markdown";
      rev = "2021-06-22";
      sha256 = "0crqn2mih21bwhhba9sm2g3lx5xla14qqz7mv04dpqf9ca998bbx";
    };
  };
  "markdown-smolder" = {
    name = "markdown-smolder";
    version = "2021-06-22";
    src = pkgs.fetchgit {
      url = "https://github.com/poorscript/purescript-markdown-smolder";
      rev = "2021-06-22";
      sha256 = "0m1jjfzwn4qz30ri9f0yj08p55l06a8mqdcw0n3rgcg0dxsvlgnm";
    };
  };
  "math" = {
    name = "math";
    version = "v3.0.0";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript/purescript-math.git";
      rev = "v3.0.0";
      sha256 = "0hkf0vyiga21992d9vbvdbnzdkvgljmsi497jjas1rk3vhblx8sq";
    };
  };
  "maybe" = {
    name = "maybe";
    version = "v5.0.0";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript/purescript-maybe.git";
      rev = "v5.0.0";
      sha256 = "0vyk3r9gklvv7awzpph7ra53zxxbin1ngmqflb5vvr2365v5xyqy";
    };
  };
  "media-types" = {
    name = "media-types";
    version = "v5.0.0";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript-contrib/purescript-media-types.git";
      rev = "v5.0.0";
      sha256 = "0l51nd1w52756pyy3zliwmhfbin0px4pxr7d2h5vchl1wq895fja";
    };
  };
  "milkis" = {
    name = "milkis";
    version = "v7.4.0";
    src = pkgs.fetchgit {
      url = "https://github.com/justinwoo/purescript-milkis.git";
      rev = "v7.4.0";
      sha256 = "10ahz4idcb3qwys7x15k3za5gkw9zbk15cdmcqsi8lfh6fg2w2db";
    };
  };
  "mmorph" = {
    name = "mmorph";
    version = "v6.0.0";
    src = pkgs.fetchgit {
      url = "https://github.com/Thimoteus/purescript-mmorph.git";
      rev = "v6.0.0";
      sha256 = "0ds88hray8v0519n9k546qsc4qs8bj1k5h5az7nwfp0gaq0r5wpk";
    };
  };
  "newtype" = {
    name = "newtype";
    version = "v4.0.0";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript/purescript-newtype.git";
      rev = "v4.0.0";
      sha256 = "1fgzbxslckva2psn0sia30hfakx8xchz3wx2kkh3w8rr4nn2py8v";
    };
  };
  "node-buffer" = {
    name = "node-buffer";
    version = "v7.0.1";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript-node/purescript-node-buffer.git";
      rev = "v7.0.1";
      sha256 = "14bf3llsa20ivkwp5hlyk8v8zfzpzhhsni9pd8rfqdyzp6zrdx3b";
    };
  };
  "node-fs" = {
    name = "node-fs";
    version = "v6.1.0";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript-node/purescript-node-fs.git";
      rev = "v6.1.0";
      sha256 = "1w97m2afn7yn757niknkbk7w6nyg4n5dabxr7gzfz368z1nkf45s";
    };
  };
  "node-path" = {
    name = "node-path";
    version = "v4.0.0";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript-node/purescript-node-path.git";
      rev = "v4.0.0";
      sha256 = "1384qyf4v84wbahafzvqdxjllqy8qkd5dpkhsl3js444vsm2aplr";
    };
  };
  "node-streams" = {
    name = "node-streams";
    version = "v5.0.0";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript-node/purescript-node-streams.git";
      rev = "v5.0.0";
      sha256 = "1jc3d4x0v77h8qcwq7hpwprsdr3gqmdfiyr1ph0kiy7r9bbrqwfx";
    };
  };
  "nonempty" = {
    name = "nonempty";
    version = "v6.0.0";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript/purescript-nonempty.git";
      rev = "v6.0.0";
      sha256 = "0azk1jrpqnjf2i97lcp63wcm31c4hddklp1mfmdan27zap3zqyjm";
    };
  };
  "now" = {
    name = "now";
    version = "v5.0.0";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript-contrib/purescript-now.git";
      rev = "v5.0.0";
      sha256 = "1wa4j2h5rlw1lgfpm7rif3v6ksm8lplxl1x69zpk8hdf0cfyz4qm";
    };
  };
  "nullable" = {
    name = "nullable";
    version = "v5.0.0";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript-contrib/purescript-nullable.git";
      rev = "v5.0.0";
      sha256 = "0jbmks8kwhpb5fr2b9nb70fjwh6zdnwirycvzr77jafcny24yrnl";
    };
  };
  "numbers" = {
    name = "numbers";
    version = "v8.0.0";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript/purescript-numbers.git";
      rev = "v8.0.0";
      sha256 = "00pm2x4kh4fm91r7nmik1v5jclkgh7gpxz13ambyqxbxbiqjq0vg";
    };
  };
  "ordered-collections" = {
    name = "ordered-collections";
    version = "v2.0.1";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript/purescript-ordered-collections.git";
      rev = "v2.0.1";
      sha256 = "1p592g0s07c56639y71782af0zz5cndpjxd5w9n41hdszsz1b86h";
    };
  };
  "orders" = {
    name = "orders";
    version = "v5.0.0";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript/purescript-orders.git";
      rev = "v5.0.0";
      sha256 = "0wwy3ycjll0s590ra35zf5gjvs86w97rln09bj428axhg7cvfl0a";
    };
  };
  "parallel" = {
    name = "parallel";
    version = "v5.0.0";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript/purescript-parallel.git";
      rev = "v5.0.0";
      sha256 = "0x8mvhgs8ygqj34xgyhk6gixqm32p2ymm00zg0zdw13g3lil9p4x";
    };
  };
  "parsing" = {
    name = "parsing";
    version = "v6.0.1";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript-contrib/purescript-parsing.git";
      rev = "v6.0.1";
      sha256 = "0vj01fw3nbgssvdl72zcjjc0gil6qcrb9irsxawsa2f5s97qq38c";
    };
  };
  "partial" = {
    name = "partial";
    version = "v3.0.0";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript/purescript-partial.git";
      rev = "v3.0.0";
      sha256 = "0acxf686hvaj793hyb7kfn9lf96kv3nk0lls2p9j095ylp55sldb";
    };
  };
  "pipes" = {
    name = "pipes";
    version = "v7.0.1";
    src = pkgs.fetchgit {
      url = "https://github.com/felixschl/purescript-pipes.git";
      rev = "v7.0.1";
      sha256 = "0jzgzi34wqqdcfgznbpfv4b8al2prd36yshnndlvkqfv70smx3kh";
    };
  };
  "precise" = {
    name = "precise";
    version = "v4.0.0";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript-contrib/purescript-precise";
      rev = "v4.0.0";
      sha256 = "18677n91h4dlfn07h7i6bwcngr6pp55w6nf4x6slq7v0n3a6xs5k";
    };
  };
  "prelude" = {
    name = "prelude";
    version = "v5.0.1";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript/purescript-prelude.git";
      rev = "v5.0.1";
      sha256 = "1x0cacvv9mmw80vy6f40y0p959q1dz28fwjswhyd7ws6npbklcy0";
    };
  };
  "profunctor" = {
    name = "profunctor";
    version = "v5.0.0";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript/purescript-profunctor.git";
      rev = "v5.0.0";
      sha256 = "0fvd2xiv77sp4jd4spgdp4i9812p6pdzzbg4pa96mbr0h19jf39c";
    };
  };
  "profunctor-lenses" = {
    name = "profunctor-lenses";
    version = "v7.0.1";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript-contrib/purescript-profunctor-lenses.git";
      rev = "v7.0.1";
      sha256 = "1wknj7g6vwk2ga1rq57l470h322308ddjn5bd3x2hhfkiy039kc3";
    };
  };
  "psci-support" = {
    name = "psci-support";
    version = "v5.0.0";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript/purescript-psci-support.git";
      rev = "v5.0.0";
      sha256 = "16vhf8hapd7rcgmafmjpiq7smhzdh3300f2idk1q4kk01yxn8ddj";
    };
  };
  "quickcheck" = {
    name = "quickcheck";
    version = "v7.1.0";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript/purescript-quickcheck.git";
      rev = "v7.1.0";
      sha256 = "1dxchng3r2mad0505a0c7cc35vs1f7y2xb5i13p59jpdz6ijqa9k";
    };
  };
  "quickcheck-laws" = {
    name = "quickcheck-laws";
    version = "v6.0.1";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript-contrib/purescript-quickcheck-laws.git";
      rev = "v6.0.1";
      sha256 = "1m397bh2w5a0wvms8rjgfxh71m7krmfkgk11j5krhz86b72k3izd";
    };
  };
  "random" = {
    name = "random";
    version = "v5.0.0";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript/purescript-random.git";
      rev = "v5.0.0";
      sha256 = "1v6ykgp8jmx488hq8mgb0l0sf1nyhjs6wq0w279iyibk9jxc6nib";
    };
  };
  "react" = {
    name = "react";
    version = "v9.0.0";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript-contrib/purescript-react.git";
      rev = "v9.0.0";
      sha256 = "1k910z3wdaik6brhbclj8dm39ic9iw93ka4z06aj8pkbfhgv3v1z";
    };
  };
  "reactix" = {
    name = "reactix";
    version = "v0.4.11";
    src = pkgs.fetchgit {
      url = "https://github.com/irresponsible/purescript-reactix";
      rev = "v0.4.11";
      sha256 = "1prxqww81lk32cp4mr318yanvf51kf9hz7x4n6rxdbcprj27710d";
    };
  };
  "read" = {
    name = "read";
    version = "v1.0.1";
    src = pkgs.fetchgit {
      url = "https://github.com/truqu/purescript-read";
      rev = "v1.0.1";
      sha256 = "0q8c1xbwh781c1jngy04lbbaq44idy33klq7q1j5ax4vzgd54z0f";
    };
  };
  "record" = {
    name = "record";
    version = "v3.0.0";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript/purescript-record.git";
      rev = "v3.0.0";
      sha256 = "0yidfvwiajiv8xflfsi2p8dqnp0qmmcz9jry58jyn9ga82z2pqn6";
    };
  };
  "record-extra" = {
    name = "record-extra";
    version = "v4.0.0";
    src = pkgs.fetchgit {
      url = "https://github.com/justinwoo/purescript-record-extra.git";
      rev = "v4.0.0";
      sha256 = "1s4xmhg8s5y6lq80j2h0a5bzni47spvzibf30w2s13p1i7v0fig5";
    };
  };
  "refs" = {
    name = "refs";
    version = "v5.0.0";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript/purescript-refs.git";
      rev = "v5.0.0";
      sha256 = "1jhc2v784jy8bvkqy4zsh2z7pnqrhwa8n5kx98xhxx73n1bf38sg";
    };
  };
  "routing" = {
    name = "routing";
    version = "v10.0.1";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript-contrib/purescript-routing.git";
      rev = "v10.0.1";
      sha256 = "0irwf1sdzxxiygxrkiyas89zwfmmjj8ql3bzab8yqbamxsp0d0na";
    };
  };
  "safe-coerce" = {
    name = "safe-coerce";
    version = "v1.0.0";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript/purescript-safe-coerce.git";
      rev = "v1.0.0";
      sha256 = "0m942lc23317izspz1sxw957mwl9yb9bgk8dh23f7b3a8w9hh8ff";
    };
  };
  "semirings" = {
    name = "semirings";
    version = "v6.0.0";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript/purescript-semirings.git";
      rev = "v6.0.0";
      sha256 = "0c53f5g6byvk92m5xs7sz734kdy721pj9yy6y51m10g6r46q3aqj";
    };
  };
  "sequences" = {
    name = "sequences";
    version = "v3.0.2";
    src = pkgs.fetchgit {
      url = "https://github.com/hdgarrood/purescript-sequences.git";
      rev = "v3.0.2";
      sha256 = "0mc0jjs1119c2nyd08yhdmliq3s47lhrdknhziga3lnbzja889k4";
    };
  };
  "simple-json" = {
    name = "simple-json";
    version = "v8.0.0";
    src = pkgs.fetchgit {
      url = "https://github.com/justinwoo/purescript-simple-json.git";
      rev = "v8.0.0";
      sha256 = "0q5hb324m1r5njnxq9wxgy99i0x8sd9mj2drq72i64xxr1k0m8qc";
    };
  };
  "simple-json-generics" = {
    name = "simple-json-generics";
    version = "v0.1.0";
    src = pkgs.fetchgit {
      url = "https://github.com/justinwoo/purescript-simple-json-generics";
      rev = "v0.1.0";
      sha256 = "1izbrh9614yi0lzpnqbn9q7hbllhvvhrgyziganj7rzgphwn3ywx";
    };
  };
  "simplecrypto" = {
    name = "simplecrypto";
    version = "v1.0.1";
    src = pkgs.fetchgit {
      url = "https://github.com/alpacaaa/purescript-simplecrypto";
      rev = "v1.0.1";
      sha256 = "0rzjzwn4s7pb8f9hm9wkl1gza9y2y9qn1116s6x5lizv81q48cyw";
    };
  };
  "smolder" = {
    name = "smolder";
    version = "v12.3.0";
    src = pkgs.fetchgit {
      url = "https://github.com/bodil/purescript-smolder";
      rev = "v12.3.0";
      sha256 = "06galacn3346ghf4w56qwj5d4z06zmlf9prd24vrvnaiwhpf42d7";
    };
  };
  "spec" = {
    name = "spec";
    version = "v5.0.1";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript-spec/purescript-spec.git";
      rev = "v5.0.1";
      sha256 = "0hpca1sa738029ww74zpw31br5x339q35kzb10iqd55lp6611k80";
    };
  };
  "spec-discovery" = {
    name = "spec-discovery";
    version = "v4.0.0";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript-spec/purescript-spec-discovery";
      rev = "v4.0.0";
      sha256 = "0kwc4nvy6rpbnhayang92h74fgyr28bh82pskwj7lm4sy513vhfw";
    };
  };
  "spec-quickcheck" = {
    name = "spec-quickcheck";
    version = "v3.1.0";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript-spec/purescript-spec-quickcheck";
      rev = "v3.1.0";
      sha256 = "1l51v2j4z2cwdnf9hyx69jmlzlkk24l8gj38iphfspaiwyqwcmpg";
    };
  };
  "st" = {
    name = "st";
    version = "v5.0.1";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript/purescript-st.git";
      rev = "v5.0.1";
      sha256 = "14hz254f1y0k3v83z719np0ddrgbca0hdsd9dvv244i07vlvm2zj";
    };
  };
  "string-parsers" = {
    name = "string-parsers";
    version = "v6.0.1";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript-contrib/purescript-string-parsers.git";
      rev = "v6.0.1";
      sha256 = "143a2s56kbx3i0xi5wfqp28znr0hdydy902jla236i7kal5y098m";
    };
  };
  "strings" = {
    name = "strings";
    version = "v5.0.0";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript/purescript-strings.git";
      rev = "v5.0.0";
      sha256 = "0hyaa4d8gyyvac2nxnwqkn2rvi5vax4bi4yv10mpk7rgb8rv7mb8";
    };
  };
  "stringutils" = {
    name = "stringutils";
    version = "v0.0.11";
    src = pkgs.fetchgit {
      url = "https://github.com/menelaos/purescript-stringutils.git";
      rev = "v0.0.11";
      sha256 = "1hbr936bvnm5iil4cfr9qhkbzd1i00yrxf5jd0rnny29df5wsq1w";
    };
  };
  "tailrec" = {
    name = "tailrec";
    version = "v5.0.1";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript/purescript-tailrec.git";
      rev = "v5.0.1";
      sha256 = "1jjl2q2hyhjcdxpamzr1cdlxhmq2bl170x5p3jajb9zgwkqx0x22";
    };
  };
  "test-unit" = {
    name = "test-unit";
    version = "v16.0.0";
    src = pkgs.fetchgit {
      url = "https://github.com/bodil/purescript-test-unit.git";
      rev = "v16.0.0";
      sha256 = "0qz903phxkgrn7qdz1xi49bydkf5cbxssyb4xk029zi4lshb35mw";
    };
  };
  "these" = {
    name = "these";
    version = "v5.0.0";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript-contrib/purescript-these.git";
      rev = "v5.0.0";
      sha256 = "0d6yg3lwgralh1kcm5cd4myyz66k9qzld61hc5dg3z92d96zbvlr";
    };
  };
  "toestand" = {
    name = "toestand";
    version = "v0.6.1";
    src = pkgs.fetchgit {
      url = "https://github.com/poorscript/purescript-toestand";
      rev = "v0.6.1";
      sha256 = "007g6h7q1d7pzh5c4dp00367kym0wycwxnryps94r6w09fyhkmis";
    };
  };
  "transformers" = {
    name = "transformers";
    version = "v5.1.0";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript/purescript-transformers.git";
      rev = "v5.1.0";
      sha256 = "15ac1jia665mglxscj3gmbg4xmlnf7xgsrkh6mvx4ayvar529xzc";
    };
  };
  "tuples" = {
    name = "tuples";
    version = "v6.0.1";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript/purescript-tuples.git";
      rev = "v6.0.1";
      sha256 = "0s2ar2gih4r34km8r8dqngh21s8899yb93mb7mips08ndy3ajq3a";
    };
  };
  "tuples-native" = {
    name = "tuples-native";
    version = "v2.2.0";
    src = pkgs.fetchgit {
      url = "https://github.com/poorscript/purescript-tuples-native";
      rev = "v2.2.0";
      sha256 = "0hplpqc2sbcjin084jqzhzqhprlc1achbqmsn9czpnf6ylgkqhaz";
    };
  };
  "type-equality" = {
    name = "type-equality";
    version = "v4.0.0";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript/purescript-type-equality.git";
      rev = "v4.0.0";
      sha256 = "126pg4zg3bsrn8dzvv75xp586nznxyswzgjlr7cag3ij3j1z0kl0";
    };
  };
  "typelevel" = {
    name = "typelevel";
    version = "v6.0.0";
    src = pkgs.fetchgit {
      url = "https://github.com/bodil/purescript-typelevel.git";
      rev = "v6.0.0";
      sha256 = "0gxj926ppx6d8inir589x0a30iv29hqc2y6vsa1n1c2vlcqv2zgd";
    };
  };
  "typelevel-prelude" = {
    name = "typelevel-prelude";
    version = "v6.0.0";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript/purescript-typelevel-prelude.git";
      rev = "v6.0.0";
      sha256 = "1vwf3yhn8mir5y41wvlyszkgd5fxvrcyfd0l8cn20c8vfq36yzgk";
    };
  };
  "typisch" = {
    name = "typisch";
    version = "v0.2.1";
    src = pkgs.fetchgit {
      url = "https://github.com/poorscript/purescript-typisch";
      rev = "v0.2.1";
      sha256 = "1jccgfayh20qsmqgbh54knl4dwbwk3144mkzzv2pi07jprp8mzbf";
    };
  };
  "uint" = {
    name = "uint";
    version = "v5.1.1";
    src = pkgs.fetchgit {
      url = "https://github.com/zaquest/purescript-uint";
      rev = "v5.1.1";
      sha256 = "13103kqj2abiy8p7v81w1dj8jm0mll177mfjb7ar6km0bsxjigc6";
    };
  };
  "unfoldable" = {
    name = "unfoldable";
    version = "v5.0.0";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript/purescript-unfoldable.git";
      rev = "v5.0.0";
      sha256 = "1v3bz04wj6hj7s6mcf49hajylg6w58n78q54sqi2ra2zq8h99kpw";
    };
  };
  "unicode" = {
    name = "unicode";
    version = "v5.0.0";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript-contrib/purescript-unicode.git";
      rev = "v5.0.0";
      sha256 = "0sqvgl3il2rl3zxkbzsqb19wib108zvyw73jxiavpfdm6hdmnxvc";
    };
  };
  "unsafe-coerce" = {
    name = "unsafe-coerce";
    version = "v5.0.0";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript/purescript-unsafe-coerce.git";
      rev = "v5.0.0";
      sha256 = "0l2agnm1k910v4yp1hz19wrsrywsr5scb397762y7pigm3frzs8r";
    };
  };
  "uri" = {
    name = "uri";
    version = "v8.0.1";
    src = pkgs.fetchgit {
      url = "https://github.com/slamdata/purescript-uri";
      rev = "v8.0.1";
      sha256 = "095svp82ik9574klx8s7vjsw34d4psda1hniqnhb75jkycspmqzw";
    };
  };
  "validation" = {
    name = "validation";
    version = "v5.0.0";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript/purescript-validation.git";
      rev = "v5.0.0";
      sha256 = "0yfb97nk7179hp0r2iylj74wl7rnl1y2x6dh5hlipxg1kpq9yydk";
    };
  };
  "variant" = {
    name = "variant";
    version = "v7.0.2";
    src = pkgs.fetchgit {
      url = "https://github.com/natefaubion/purescript-variant.git";
      rev = "v7.0.2";
      sha256 = "0a555fa2d8kd6rzfv9w64aphr7n6x0cizfp7n71wh5jw07b7hn5y";
    };
  };
  "versions" = {
    name = "versions";
    version = "v6.0.0";
    src = pkgs.fetchgit {
      url = "https://github.com/hdgarrood/purescript-versions.git";
      rev = "v6.0.0";
      sha256 = "0653dgdf44nb6ribm8m9lnzjvrmscqm3fqd7ys7q38k1388z706y";
    };
  };
  "web-dom" = {
    name = "web-dom";
    version = "v5.0.0";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript-web/purescript-web-dom.git";
      rev = "v5.0.0";
      sha256 = "06g9cp9fkzyfwbz5cs0wxjxgdydm9hy7756p2w4vx94myki20hgx";
    };
  };
  "web-events" = {
    name = "web-events";
    version = "v3.0.0";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript-web/purescript-web-events.git";
      rev = "v3.0.0";
      sha256 = "1dxwrl2r39vazb3g1ka4dkpy6idyi17aq4hf9vvdsmcwf2jjwbn9";
    };
  };
  "web-file" = {
    name = "web-file";
    version = "v3.0.0";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript-web/purescript-web-file.git";
      rev = "v3.0.0";
      sha256 = "11x1inhr5pvs2iyg818cywwdskb33q777592sd3b4g4jyczcb1li";
    };
  };
  "web-html" = {
    name = "web-html";
    version = "v3.1.0";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript-web/purescript-web-html.git";
      rev = "v3.1.0";
      sha256 = "007anmqqifrjnpfa4xf7qa8xnpbhvcxqdraj9lnhizwq65vx53sn";
    };
  };
  "web-storage" = {
    name = "web-storage";
    version = "v4.0.0";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript-web/purescript-web-storage.git";
      rev = "v4.0.0";
      sha256 = "1viy027k9qyr7mckqkvizwbwkfskc6frppsa1v9a0hq6gc08mpjx";
    };
  };
  "web-xhr" = {
    name = "web-xhr";
    version = "v4.1.0";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript-web/purescript-web-xhr.git";
      rev = "v4.1.0";
      sha256 = "0hzmqga8l24l20kyd98bcpd8bmz7by14vl311m9yfdg5mjkjg42g";
    };
  };
}
