{ pkgs ? import <nixpkgs> {} }:

{
  "aff" = {
    name = "aff";
    version = "v5.1.1";
    src = pkgs.fetchgit {
      url = "https://github.com/slamdata/purescript-aff.git";
      rev = "v5.1.1";
      sha256 = "19v5psd6jz13gr5yqyx8286b5bpfq1dax51w906y0mqgnfz92yzr";
    };
  };
  "aff-promise" = {
    name = "aff-promise";
    version = "v2.1.0";
    src = pkgs.fetchgit {
      url = "https://github.com/nwolverson/purescript-aff-promise.git";
      rev = "v2.1.0";
      sha256 = "0khm53lvxgvc7fbsvcr2h2wlhcgay8vq45755f0w8vpk1441dvww";
    };
  };
  "affjax" = {
    name = "affjax";
    version = "v9.0.0";
    src = pkgs.fetchgit {
      url = "https://github.com/slamdata/purescript-affjax.git";
      rev = "v9.0.0";
      sha256 = "04xij3q4802z58mzaaz661ifv1rd673fq2mjqj553dw326zplpvs";
    };
  };
  "ansi" = {
    name = "ansi";
    version = "v5.0.0";
    src = pkgs.fetchgit {
      url = "https://github.com/hdgarrood/purescript-ansi.git";
      rev = "v5.0.0";
      sha256 = "16kgy9zbdms9appd69gad2bka44ijkcnc9p0kf5g7x672jypx3ar";
    };
  };
  "argonaut" = {
    name = "argonaut";
    version = "v6.0.0";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript-contrib/purescript-argonaut.git";
      rev = "v6.0.0";
      sha256 = "0b2jy5sf2lnn1r4kxnnm2vsr5iqkfas0jqp9yrrjmg7qngkd6qkw";
    };
  };
  "argonaut-codecs" = {
    name = "argonaut-codecs";
    version = "v6.0.2";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript-contrib/purescript-argonaut-codecs.git";
      rev = "v6.0.2";
      sha256 = "1i8dm95kbcl7k92jqnylhng1rjcqx5z12yhf6mwvp0z0bmsz250b";
    };
  };
  "argonaut-core" = {
    name = "argonaut-core";
    version = "v5.0.0";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript-contrib/purescript-argonaut-core.git";
      rev = "v5.0.0";
      sha256 = "1d3jgyap3vwc7mc01ji6brmd2n8lr9mmgx0fl88yalrm2j89jrcc";
    };
  };
  "argonaut-traversals" = {
    name = "argonaut-traversals";
    version = "v7.0.0";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript-contrib/purescript-argonaut-traversals.git";
      rev = "v7.0.0";
      sha256 = "0mr6c7c64c5b7ia0b68vsjyjxw06ac9908xpgxdd969kpvcwq9x3";
    };
  };
  "arraybuffer-types" = {
    name = "arraybuffer-types";
    version = "v2.0.0";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript-contrib/purescript-arraybuffer-types.git";
      rev = "v2.0.0";
      sha256 = "059a8n49yhl46l1b1j2qj4ichzq6dzj29ajkfvw88npzj5w2rshy";
    };
  };
  "arrays" = {
    name = "arrays";
    version = "v5.3.0";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript/purescript-arrays.git";
      rev = "v5.3.0";
      sha256 = "0r991qcx69xj07iagjaxkm0ab9qi2pw8cs3qwyybhis7asd6bnz6";
    };
  };
  "avar" = {
    name = "avar";
    version = "v3.0.0";
    src = pkgs.fetchgit {
      url = "https://github.com/slamdata/purescript-avar.git";
      rev = "v3.0.0";
      sha256 = "14g05jm2xricy5b9vn4k4lhc9lzi5jvpvv85h10s17kn4wwi9igk";
    };
  };
  "bifunctors" = {
    name = "bifunctors";
    version = "v4.0.0";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript/purescript-bifunctors.git";
      rev = "v4.0.0";
      sha256 = "1bdra5fbkraglqrrm484vw8h0wwk48kzkn586v4y7fg106q1q386";
    };
  };
  "catenable-lists" = {
    name = "catenable-lists";
    version = "v5.0.1";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript/purescript-catenable-lists.git";
      rev = "v5.0.1";
      sha256 = "0mbpb8xr9a7a4bvawhki7js5cbv7c0lv0vdwb6r8nmv6b61gzg27";
    };
  };
  "colors" = {
    name = "colors";
    version = "v5.0.0";
    src = pkgs.fetchgit {
      url = "https://github.com/sharkdp/purescript-colors.git";
      rev = "v5.0.0";
      sha256 = "05bkfqllfpkh7nj0nzgd5p387hlpk0x35nam1i6xm3vzma9csj18";
    };
  };
  "console" = {
    name = "console";
    version = "v4.2.0";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript/purescript-console.git";
      rev = "v4.2.0";
      sha256 = "1b2nykdq1dzaqyra5pi8cvvz4dsbbkg57a2c88yi931ynm19nnw6";
    };
  };
  "const" = {
    name = "const";
    version = "v4.1.0";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript/purescript-const.git";
      rev = "v4.1.0";
      sha256 = "0qbd2aisax52yw6sybdhs7na943cbsd6mylhhgsamrf7hzh6v51p";
    };
  };
  "contravariant" = {
    name = "contravariant";
    version = "v4.0.1";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript/purescript-contravariant.git";
      rev = "v4.0.1";
      sha256 = "0dd17lwlybpz4i54cxnqdgy38rjlbw9p7bw1r43is6z3kdc8983a";
    };
  };
  "control" = {
    name = "control";
    version = "v4.1.0";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript/purescript-control.git";
      rev = "v4.1.0";
      sha256 = "10703zvsnjm5fc74k6wzjsvpsfyc3jci3jxhm7rxf7ymya9z1z53";
    };
  };
  "coroutines" = {
    name = "coroutines";
    version = "v5.0.0";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript-contrib/purescript-coroutines.git";
      rev = "v5.0.0";
      sha256 = "1jax7by8kn9fjg00avhziy3n18i3510iwzs3d73ziplanbw4qw4k";
    };
  };
  "css" = {
    name = "css";
    version = "v4.0.0";
    src = pkgs.fetchgit {
      url = "https://github.com/slamdata/purescript-css.git";
      rev = "v4.0.0";
      sha256 = "0f6gib6rp20qz08vramw7k6kv2ck315lmshjpii8zmkjb5ya0w55";
    };
  };
  "datetime" = {
    name = "datetime";
    version = "v4.1.1";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript/purescript-datetime.git";
      rev = "v4.1.1";
      sha256 = "06ghfvbvd5sd0q014qi8j8glk2g2j9f8z8cwq2318kllp92gz07q";
    };
  };
  "debug" = {
    name = "debug";
    version = "v4.0.0";
    src = pkgs.fetchgit {
      url = "https://github.com/garyb/purescript-debug.git";
      rev = "v4.0.0";
      sha256 = "0gwjj80akys0h111i74n429fmny992gx0r4rk1n98gqlqm5cmi21";
    };
  };
  "distributive" = {
    name = "distributive";
    version = "v4.0.0";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript/purescript-distributive.git";
      rev = "v4.0.0";
      sha256 = "0zbn0yq1vv7fbbf1lncg80irz0vg7wnw9b9wrzxhdzpbkw4jinsl";
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
    version = "v2.0.1";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript/purescript-effect.git";
      rev = "v2.0.1";
      sha256 = "1vqw5wrdxzh1ww6z7271xr4kg4mx0r3k3mwg18dmgmzj11wbn2wh";
    };
  };
  "either" = {
    name = "either";
    version = "v4.1.1";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript/purescript-either.git";
      rev = "v4.1.1";
      sha256 = "12j7vvjly0bgxidxmb2pflx0zy7x425dnvxk2y1pm66n0hbsq7ns";
    };
  };
  "enums" = {
    name = "enums";
    version = "v4.0.1";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript/purescript-enums.git";
      rev = "v4.0.1";
      sha256 = "0qq0pgmq497nfml0y8xb2qdpga5xqp9sqq4ilv1rpyhg1v7nzb6v";
    };
  };
  "exceptions" = {
    name = "exceptions";
    version = "v4.0.0";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript/purescript-exceptions.git";
      rev = "v4.0.0";
      sha256 = "17s0rg9k4phivhx9j3l2vqqfdhk61gpj1xfqy8w6zj3rnxj0b2cv";
    };
  };
  "exists" = {
    name = "exists";
    version = "v4.0.0";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript/purescript-exists.git";
      rev = "v4.0.0";
      sha256 = "0bbdbw3jaqyi8dj2d52zvfgx4vl84d8cr6hp43vy8lfjfcbj0wlk";
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
    version = "v4.1.1";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript/purescript-foldable-traversable.git";
      rev = "v4.1.1";
      sha256 = "03x89xcmphckzjlp4chc7swrpw347ll5bvr2yp7x9v2jqw2jlyi1";
    };
  };
  "foreign" = {
    name = "foreign";
    version = "v5.0.0";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript/purescript-foreign.git";
      rev = "v5.0.0";
      sha256 = "15mz2s4f8crkd721z4df2aag4s0wil6fs07cpcmi7dpnkn7a4nab";
    };
  };
  "foreign-generic" = {
    name = "foreign-generic";
    version = "v10.0.0";
    src = pkgs.fetchgit {
      url = "https://github.com/paf31/purescript-foreign-generic.git";
      rev = "v10.0.0";
      sha256 = "1assvgmnim908plv7wzz4mrvq3lh4ayimgz5xk5njyz8p6pzkyvl";
    };
  };
  "foreign-object" = {
    name = "foreign-object";
    version = "v2.0.3";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript/purescript-foreign-object.git";
      rev = "v2.0.3";
      sha256 = "07wiql59zfj901nk9526b6rykn9m24jjcs8v5dgxbr1c3hiab9n3";
    };
  };
  "fork" = {
    name = "fork";
    version = "v4.0.0";
    src = pkgs.fetchgit {
      url = "https://github.com/slamdata/purescript-fork.git";
      rev = "v4.0.0";
      sha256 = "1jygqzyci40c28gw2ygnx8v52hilhajj1bdpn7ndvss4i7yh1lcp";
    };
  };
  "form-urlencoded" = {
    name = "form-urlencoded";
    version = "v4.0.1";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript-contrib/purescript-form-urlencoded.git";
      rev = "v4.0.1";
      sha256 = "14nb7mslmbms4wn4j6a7nrz8xmnach3qgcf8i1hgql1c8j7zkqz8";
    };
  };
  "free" = {
    name = "free";
    version = "v5.2.0";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript/purescript-free.git";
      rev = "v5.2.0";
      sha256 = "1bwj0ay7q9lm4ir29jy549m05jvaqik1s615biv23y51ngx3fn49";
    };
  };
  "freet" = {
    name = "freet";
    version = "v4.0.0";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript-contrib/purescript-freet.git";
      rev = "v4.0.0";
      sha256 = "1kpgggwimxjvdvhn6s7z4lzg3yw1rqg5f7pyamx9gh232s4va40i";
    };
  };
  "functions" = {
    name = "functions";
    version = "v4.0.0";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript/purescript-functions.git";
      rev = "v4.0.0";
      sha256 = "0675k5kxxwdvsjs6a3is8pwm3hmv0vbcza1b8ls10gymmfz3k3hj";
    };
  };
  "functors" = {
    name = "functors";
    version = "v3.1.1";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript/purescript-functors.git";
      rev = "v3.1.1";
      sha256 = "1cnn3zhk6qcyiwbbpvrdqgsbch4qihx2y9d6sv45bvd8kzrbd306";
    };
  };
  "gen" = {
    name = "gen";
    version = "v2.1.1";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript/purescript-gen.git";
      rev = "v2.1.1";
      sha256 = "0pk68cn1s89hql30ydks9gh34gbxg391smi2albx3qvxnfkrpxca";
    };
  };
  "generics-rep" = {
    name = "generics-rep";
    version = "v6.1.1";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript/purescript-generics-rep.git";
      rev = "v6.1.1";
      sha256 = "15vchzbcvf6byks90q14lvcwb8hnxqzm2mrlxi7v1f7has4s74kn";
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
    version = "v4.0.2";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript-contrib/purescript-http-methods.git";
      rev = "v4.0.2";
      sha256 = "1wfgrlnl33bcqw54hbc8icah2fi0rvi5zxhz07665vy9p5ppvkqs";
    };
  };
  "identity" = {
    name = "identity";
    version = "v4.1.0";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript/purescript-identity.git";
      rev = "v4.1.0";
      sha256 = "1scdgbfkphfmapw7p9rnsiynpmqzyvnal2glzj450q51f8g1dhld";
    };
  };
  "integers" = {
    name = "integers";
    version = "v4.0.0";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript/purescript-integers.git";
      rev = "v4.0.0";
      sha256 = "17d4bfpnrmbxlc7hhhrvnli70ydaqyr26zgvc9q52a76zgdcb4cf";
    };
  };
  "invariant" = {
    name = "invariant";
    version = "v4.1.0";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript/purescript-invariant.git";
      rev = "v4.1.0";
      sha256 = "1fimpbh3yb7clvqxcdf4yf9im64z0v2s9pbspfacgq5b4vshjas9";
    };
  };
  "js-date" = {
    name = "js-date";
    version = "v6.0.0";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript-contrib/purescript-js-date.git";
      rev = "v6.0.0";
      sha256 = "19qyzbr4a1ca8znbd8gcbz0a801f5p1v238ky3408gdx4ba32zjd";
    };
  };
  "js-timers" = {
    name = "js-timers";
    version = "v4.0.1";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript-contrib/purescript-js-timers.git";
      rev = "v4.0.1";
      sha256 = "1a8092sli7zqw1wflibhjza1ww21dxl7x7r602iazzwh2g70v4dg";
    };
  };
  "lazy" = {
    name = "lazy";
    version = "v4.0.0";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript/purescript-lazy.git";
      rev = "v4.0.0";
      sha256 = "156q89l4nvvn14imbhp6xvvm82q7kqh1pyndmldmnkhiqyr84vqv";
    };
  };
  "lcg" = {
    name = "lcg";
    version = "v2.0.0";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript/purescript-lcg.git";
      rev = "v2.0.0";
      sha256 = "1851cq2g84jzjbvbmncbivbhaqzj9zv5ni3gs14k04nmx2brxmvj";
    };
  };
  "lists" = {
    name = "lists";
    version = "v5.4.1";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript/purescript-lists.git";
      rev = "v5.4.1";
      sha256 = "0l0jiqhcjzkg4nv2a6h2vdf4izr9mf7cxc86cq1hf3j4dh6spym1";
    };
  };
  "markdown" = {
    name = "markdown";
    version = "2020-03-04";
    src = pkgs.fetchgit {
      url = "https://github.com/poorscript/purescript-markdown";
      rev = "2020-03-04";
      sha256 = "0xp41wg1p4dwivgpy121mzpimkdakg0m83hx8ypb5ayjk98vvyf0";
    };
  };
  "markdown-smolder" = {
    name = "markdown-smolder";
    version = "2020-03-04";
    src = pkgs.fetchgit {
      url = "https://github.com/poorscript/purescript-markdown-smolder";
      rev = "2020-03-04";
      sha256 = "1ls258cbmcv6sx516ppwwnxsrm6ydz3jm0q4lz83f57sryfa01b2";
    };
  };
  "math" = {
    name = "math";
    version = "v2.1.1";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript/purescript-math.git";
      rev = "v2.1.1";
      sha256 = "1msmy9w7y6fij62sdc55w68gpwkhm6lhgc8qjisjk4sxx1wdg1rr";
    };
  };
  "maybe" = {
    name = "maybe";
    version = "v4.0.1";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript/purescript-maybe.git";
      rev = "v4.0.1";
      sha256 = "073wa0d51daxdwacfcxg5pf6ch63n4ii55xm1ih87bxrg8mp52mx";
    };
  };
  "media-types" = {
    name = "media-types";
    version = "v4.0.1";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript-contrib/purescript-media-types.git";
      rev = "v4.0.1";
      sha256 = "0ykwmxrhmwfy6c5mxjxa43xdf5xqakrqyvr5fn986yad50gjqj75";
    };
  };
  "milkis" = {
    name = "milkis";
    version = "v7.2.0";
    src = pkgs.fetchgit {
      url = "https://github.com/justinwoo/purescript-milkis.git";
      rev = "v7.2.0";
      sha256 = "1i6l8k4clrnq4wxbdzzq08kagk591yr24xip86z1j7xap98y02xg";
    };
  };
  "mmorph" = {
    name = "mmorph";
    version = "v5.1.0";
    src = pkgs.fetchgit {
      url = "https://github.com/Thimoteus/purescript-mmorph.git";
      rev = "v5.1.0";
      sha256 = "1lvdclqi9wzs4j8xj8ygnj2b87hhpbnl0c6m28zac05rz87s09mg";
    };
  };
  "newtype" = {
    name = "newtype";
    version = "v3.0.0";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript/purescript-newtype.git";
      rev = "v3.0.0";
      sha256 = "0qvk9p41miy806b05b4ikbr3if0fcyc35gfrwd2mflcxxp46011c";
    };
  };
  "node-buffer" = {
    name = "node-buffer";
    version = "v5.0.0";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript-node/purescript-node-buffer.git";
      rev = "v5.0.0";
      sha256 = "0ih2y29srdxgn526fw2v1y95hpivjil44vkl93w6nrqsymki36y0";
    };
  };
  "node-fs" = {
    name = "node-fs";
    version = "v5.0.1";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript-node/purescript-node-fs.git";
      rev = "v5.0.1";
      sha256 = "0i3bd7aw16vyb5sh5fzlvgg9q6cjdvmnfs57in6rxm34z8d8c0p8";
    };
  };
  "node-path" = {
    name = "node-path";
    version = "v3.0.0";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript-node/purescript-node-path.git";
      rev = "v3.0.0";
      sha256 = "0j1ni52m62dpcrfakl1ik131i31bkg91yv0p1c40sdw0f59fzf6x";
    };
  };
  "node-streams" = {
    name = "node-streams";
    version = "v4.0.1";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript-node/purescript-node-streams.git";
      rev = "v4.0.1";
      sha256 = "12dki2li0d7s54kvcr6gksb5nxf6kzs93gwxnq4bh1flri8p0i7r";
    };
  };
  "nonempty" = {
    name = "nonempty";
    version = "v5.0.0";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript/purescript-nonempty.git";
      rev = "v5.0.0";
      sha256 = "1vz174sg32cqrp52nwb2vz9frrzmdwzzlgl4vc2cm5wlf2anirxj";
    };
  };
  "now" = {
    name = "now";
    version = "v4.0.0";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript-contrib/purescript-now.git";
      rev = "v4.0.0";
      sha256 = "18h5pif2dy4r7w1xg2zw4mvdqlar9xqp3rawkiavmsc946ncf3zs";
    };
  };
  "nullable" = {
    name = "nullable";
    version = "v4.1.1";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript-contrib/purescript-nullable.git";
      rev = "v4.1.1";
      sha256 = "14qaxxga8gqlr4pijyvqycdyhjr6qpz3h4aarficw5j75b7x8nyv";
    };
  };
  "numbers" = {
    name = "numbers";
    version = "v7.0.0";
    src = pkgs.fetchgit {
      url = "https://github.com/sharkdp/purescript-numbers.git";
      rev = "v7.0.0";
      sha256 = "1l9s22fkjf7wc0zd3wjax4hlif7gqh6ij0ijb8sq20mfh2xl4hj8";
    };
  };
  "ordered-collections" = {
    name = "ordered-collections";
    version = "v1.6.1";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript/purescript-ordered-collections.git";
      rev = "v1.6.1";
      sha256 = "0r48p94d3cyzni2z7ikzcap472k23dx8zq37c1prmjb01v03mfvc";
    };
  };
  "orders" = {
    name = "orders";
    version = "v4.0.0";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript/purescript-orders.git";
      rev = "v4.0.0";
      sha256 = "13p1sm4dxkmxhld9x5qqg62iiajjb3qpzs66c1r24y5fs4zsfry4";
    };
  };
  "parallel" = {
    name = "parallel";
    version = "v4.0.0";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript/purescript-parallel.git";
      rev = "v4.0.0";
      sha256 = "1d5bnagabw2k8yxywkygwrpblb2ggqh2fhpqfrx2sj1y53x332hg";
    };
  };
  "parsing" = {
    name = "parsing";
    version = "v5.0.3";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript-contrib/purescript-parsing.git";
      rev = "v5.0.3";
      sha256 = "0m5xvb5kis28laj3navyyakyq408vw115c2dvngf1vljzh1hk5kj";
    };
  };
  "partial" = {
    name = "partial";
    version = "v2.0.1";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript/purescript-partial.git";
      rev = "v2.0.1";
      sha256 = "11qr80989g7xmvw1brnrclsbg2wdkbr5k3cqpngfip3nnirrypcn";
    };
  };
  "pipes" = {
    name = "pipes";
    version = "v6.0.0";
    src = pkgs.fetchgit {
      url = "https://github.com/felixSchl/purescript-pipes.git";
      rev = "v6.0.0";
      sha256 = "0vl37f42dy4w4hswiay22w0n14k7sr670x54iwn7428larzrzs8y";
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
    version = "v4.1.1";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript/purescript-prelude.git";
      rev = "v4.1.1";
      sha256 = "1frvjrv0mr508r6683l1mp5rzm1y2dz76fz40zf4k2c64sy6y1xm";
    };
  };
  "profunctor" = {
    name = "profunctor";
    version = "v4.0.0";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript/purescript-profunctor.git";
      rev = "v4.0.0";
      sha256 = "1v4kvmhmiwznd4lswp9339h64pgv5zvd3vm1q7gzj70767a3941i";
    };
  };
  "profunctor-lenses" = {
    name = "profunctor-lenses";
    version = "v6.2.0";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript-contrib/purescript-profunctor-lenses.git";
      rev = "v6.2.0";
      sha256 = "0bb8ib00in8d65kvympdlfncb7bp1qqsjvvpw9zfwiwf67ix4v57";
    };
  };
  "proxy" = {
    name = "proxy";
    version = "v3.0.0";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript/purescript-proxy.git";
      rev = "v3.0.0";
      sha256 = "0rqf25b1n9p5sgx7gdsxwrfv9rb3sqxgqmqpp5kdm30lfk7snz24";
    };
  };
  "psci-support" = {
    name = "psci-support";
    version = "v4.0.0";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript/purescript-psci-support.git";
      rev = "v4.0.0";
      sha256 = "0jd773zcklr6hjddqingzmk20a0afpm2r9pczfjbj0d93pbxb4xh";
    };
  };
  "quickcheck" = {
    name = "quickcheck";
    version = "v6.1.0";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript/purescript-quickcheck.git";
      rev = "v6.1.0";
      sha256 = "0b6208067krf81kzq2hbxs68386hcicbscwxbj5nck07ivjjvqh0";
    };
  };
  "random" = {
    name = "random";
    version = "v4.0.0";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript/purescript-random.git";
      rev = "v4.0.0";
      sha256 = "0k37v7z529adx6ypxj0xjyfrz45qia6p0vki2wpvi8lik7c698gf";
    };
  };
  "react" = {
    name = "react";
    version = "v7.0.1";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript-contrib/purescript-react.git";
      rev = "v7.0.1";
      sha256 = "0kym675db8i4wzd0rh1f17k4mcfbf4pwnhpf7zhzkm9zaj7wc2cg";
    };
  };
  "react-dom" = {
    name = "react-dom";
    version = "v6.1.0";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript-contrib/purescript-react-dom.git";
      rev = "v6.1.0";
      sha256 = "19kzsahx3kvgbi9bhnnz50fjmqvvgslsg6rk028bj4v28m8gra40";
    };
  };
  "reactix" = {
    name = "reactix";
    version = "v0.4.5";
    src = pkgs.fetchgit {
      url = "https://github.com/irresponsible/purescript-reactix";
      rev = "v0.4.5";
      sha256 = "0nzings4ygsrl4ak8x87sns7lgv6fljzhw69l0gk1iihhhazkhan";
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
    version = "v2.0.1";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript/purescript-record.git";
      rev = "v2.0.1";
      sha256 = "1l7ixb0gc2man36181g3hdf46sjp7xh0kv8bgrvalxfisjmd12v0";
    };
  };
  "record-extra" = {
    name = "record-extra";
    version = "v3.0.0";
    src = pkgs.fetchgit {
      url = "https://github.com/justinwoo/purescript-record-extra.git";
      rev = "v3.0.0";
      sha256 = "1zhsgafd02h7161xldnf4z3sfjacp2if8py157igiszkjxaipvdd";
    };
  };
  "refs" = {
    name = "refs";
    version = "v4.1.0";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript/purescript-refs.git";
      rev = "v4.1.0";
      sha256 = "08161iy1xbafzblv033v84156azpcqkiw9v9d6gliphrq5fm17gm";
    };
  };
  "routing" = {
    name = "routing";
    version = "v9.0.0";
    src = pkgs.fetchgit {
      url = "https://github.com/slamdata/purescript-routing.git";
      rev = "v9.0.0";
      sha256 = "1q05k1nrwpmlwy35fwa6kp1drk1ywnf5srmm1rzpliprvpli3is7";
    };
  };
  "semirings" = {
    name = "semirings";
    version = "v5.0.0";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript/purescript-semirings.git";
      rev = "v5.0.0";
      sha256 = "0bhrhn2yvcgil7g63spb2xw966mdhlk9mpspnqfijdpb9n3b79ds";
    };
  };
  "sequences" = {
    name = "sequences";
    version = "v2.1.0";
    src = pkgs.fetchgit {
      url = "https://github.com/hdgarrood/purescript-sequences.git";
      rev = "v2.1.0";
      sha256 = "10fkkmmb7qh4p5gmgb6xpxh9g8hy06ddy8cyfrs3py8a5b8h46hw";
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
    version = "v12.0.0";
    src = pkgs.fetchgit {
      url = "https://github.com/bodil/purescript-smolder.git";
      rev = "v12.0.0";
      sha256 = "1v8707hgm9g4k30afc30cxzfcz627bmxf7cn9wk3l3mh9ppl8kwd";
    };
  };
  "spec" = {
    name = "spec";
    version = "v4.0.0";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript-spec/purescript-spec.git";
      rev = "v4.0.0";
      sha256 = "1b1hw3231ha5fq61948dfz28a0lm2mcn2pksszf7yw3137ysp5bn";
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
    version = "v4.0.0";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript/purescript-st.git";
      rev = "v4.0.0";
      sha256 = "0m2jkb9dmpbr8s1c20l7sm2q11y5rx8gqfiyspnyhq5apzkknjr0";
    };
  };
  "string-parsers" = {
    name = "string-parsers";
    version = "v5.0.0";
    src = pkgs.fetchgit {
      url = "https://github.com/paf31/purescript-string-parsers.git";
      rev = "v5.0.0";
      sha256 = "1lykswyd3icv9lx4r096lxfd244i0pzj2f4fpm604czzlfw6bspv";
    };
  };
  "strings" = {
    name = "strings";
    version = "v4.0.1";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript/purescript-strings.git";
      rev = "v4.0.1";
      sha256 = "147l3l3fzn7liparhm2y3p8j4ck1lblra5j493p2hy5yy5ndznc8";
    };
  };
  "stringutils" = {
    name = "stringutils";
    version = "v0.0.9";
    src = pkgs.fetchgit {
      url = "https://github.com/menelaos/purescript-stringutils.git";
      rev = "v0.0.9";
      sha256 = "00c679d5lk37gbfby2kasyq45rla6a0shg64zhyd2r4xrj0a74r4";
    };
  };
  "tailrec" = {
    name = "tailrec";
    version = "v4.0.0";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript/purescript-tailrec.git";
      rev = "v4.0.0";
      sha256 = "0z7k80nl8dgv8mc2w8xsl2n0637bd1l8ppxak8kaifgjjwa81hx3";
    };
  };
  "thermite" = {
    name = "thermite";
    version = "hide-2020-03-04";
    src = pkgs.fetchgit {
      url = "https://github.com/poorscript/purescript-thermite.git";
      rev = "hide-2020-03-04";
      sha256 = "1s0fj6f7kqafiw027yw65f5193kph51x6dxw0wv3g6j77yxmk92i";
    };
  };
  "these" = {
    name = "these";
    version = "v4.0.0";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript-contrib/purescript-these.git";
      rev = "v4.0.0";
      sha256 = "0ywwpbcz1d0pdi3f9h9kla52vq1if8zwdz7jq7lqz5s8zj8kyg5r";
    };
  };
  "transformers" = {
    name = "transformers";
    version = "v4.2.0";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript/purescript-transformers.git";
      rev = "v4.2.0";
      sha256 = "03qmvl9s7lbvm73dy9ps6qp39pdcm91hb8yakgj7aq8hgpj7b6bg";
    };
  };
  "tuples" = {
    name = "tuples";
    version = "v5.1.0";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript/purescript-tuples.git";
      rev = "v5.1.0";
      sha256 = "045nsy0r2g51gih0wjhcvhl6gfr8947mlrqwg644ygz72rjm8wq4";
    };
  };
  "tuples-native" = {
    name = "tuples-native";
    version = "v2.0.1";
    src = pkgs.fetchgit {
      url = "https://github.com/athanclark/purescript-tuples-native";
      rev = "v2.0.1";
      sha256 = "1c8065krignnphiwnws9d5ingfx8k83wqnmd1zadyjlakfdg2b4h";
    };
  };
  "type-equality" = {
    name = "type-equality";
    version = "v3.0.0";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript/purescript-type-equality.git";
      rev = "v3.0.0";
      sha256 = "1b7szyca5s96gaawvgwrw7fa8r7gqsdff7xhz3vvngnylv2scl3w";
    };
  };
  "typelevel" = {
    name = "typelevel";
    version = "v5.0.0";
    src = pkgs.fetchgit {
      url = "https://github.com/bodil/purescript-typelevel.git";
      rev = "v5.0.0";
      sha256 = "1s0fdffmn386hahv670fc5vvj0x0cqk64i4jj95rfavffch4x7va";
    };
  };
  "typelevel-prelude" = {
    name = "typelevel-prelude";
    version = "v5.0.0";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript/purescript-typelevel-prelude.git";
      rev = "v5.0.0";
      sha256 = "01ki39xj87kwf8j8divlzwrvfyjcgxpmzhhmxzjylqx6jarcwyg6";
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
    version = "v4.0.2";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript/purescript-unfoldable.git";
      rev = "v4.0.2";
      sha256 = "0lzhx030c5933maxcjwk6kzlvbxky1kiwrymqf1dp5wbrar3jyv4";
    };
  };
  "unicode" = {
    name = "unicode";
    version = "v4.0.1";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript-contrib/purescript-unicode.git";
      rev = "v4.0.1";
      sha256 = "1a53jv7pzyjk5v6kmwwy50d3l8d26k0id59sn8g3lzkih24nalhp";
    };
  };
  "unsafe-coerce" = {
    name = "unsafe-coerce";
    version = "v4.0.0";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript/purescript-unsafe-coerce.git";
      rev = "v4.0.0";
      sha256 = "0k9255mk2mz6xjb11pwkgfcblmmyvr86ig5kr92jwy95xim09zip";
    };
  };
  "uri" = {
    name = "uri";
    version = "v7.0.0";
    src = pkgs.fetchgit {
      url = "https://github.com/slamdata/purescript-uri";
      rev = "v7.0.0";
      sha256 = "1ry5h5656k2hn3y5s35y7pz0rngbkvj9jc783i4h9ai3hndi00py";
    };
  };
  "validation" = {
    name = "validation";
    version = "v4.2.0";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript/purescript-validation.git";
      rev = "v4.2.0";
      sha256 = "03irk6n7jgsimhp9ckrg2ns7qbc8d383ls3sslxgir5mr8xdc44g";
    };
  };
  "versions" = {
    name = "versions";
    version = "v5.0.1";
    src = pkgs.fetchgit {
      url = "https://github.com/hdgarrood/purescript-versions.git";
      rev = "v5.0.1";
      sha256 = "07h2s3411w9d0iany7arw01qvfj57wgj8pgfqvrm0vvvhhc0v9f6";
    };
  };
  "web-dom" = {
    name = "web-dom";
    version = "v3.0.0";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript-web/purescript-web-dom.git";
      rev = "v3.0.0";
      sha256 = "14mssfhg81diy82wbv3ja8zk9a92267c6svq4bgfag91hydw4p42";
    };
  };
  "web-events" = {
    name = "web-events";
    version = "v2.0.1";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript-web/purescript-web-events.git";
      rev = "v2.0.1";
      sha256 = "1vd1gfh6zv50bq4v1ppl2wvc5mskcw9n1bfj29qjg0dx0ffxzv2f";
    };
  };
  "web-file" = {
    name = "web-file";
    version = "v2.1.1";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript-web/purescript-web-file.git";
      rev = "v2.1.1";
      sha256 = "08k0xv9zpg040v32k3wk66bhzx16c2nn9v35f01gq96anhm713s1";
    };
  };
  "web-html" = {
    name = "web-html";
    version = "v2.2.1";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript-web/purescript-web-html.git";
      rev = "v2.2.1";
      sha256 = "026y4kkrc2z4zmxlc4z21znq8knaqqjazf1ny811gm1507b1ykc6";
    };
  };
  "web-storage" = {
    name = "web-storage";
    version = "v3.0.0";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript-web/purescript-web-storage.git";
      rev = "v3.0.0";
      sha256 = "1ycb2s29aw9w6lqik6hfmp9nf9i2yhn0q26hc4p3450jam5mj8bx";
    };
  };
  "web-xhr" = {
    name = "web-xhr";
    version = "v3.0.1";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript-web/purescript-web-xhr.git";
      rev = "v3.0.1";
      sha256 = "1217lmvla3rqd9kzvvjxpngj7nxjn2ianm9xhlc0iydzsd0agihi";
    };
  };
}
