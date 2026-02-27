import { useState, useRef, useEffect, useCallback } from "react";

// ═══════════════════════════════════════════
// GAME DATA
// ═══════════════════════════════════════════

const DICE_CONFIG = [
  ["A","E","I","O","A","U"],["E","A","O","I","U","E"],
  ["R","S","T","N","L","E"],["D","H","N","S","T","R"],
  ["C","M","P","G","L","A"],["B","F","W","Y","K","O"],
  ["N","V","J","Qu","X","Z"],
];

const LETTER_VALUES = {
  A:1,E:1,I:1,O:1,U:1,N:1,R:1,S:1,T:1,L:1,
  D:2,G:2,H:2,C:2,M:2,B:3,F:3,P:3,W:3,Y:3,
  K:5,V:5,J:8,X:8,Qu:10,Z:10,
};

const COMPOUND_WORDS = new Set([
  "airway","armpit","backlog","backup","bathrobe","bedrock","bedbug","bellboy",
  "blackout","blowout","boldface","bookcase","bookmark","cannot","catnap","catnip",
  "cobweb","comeback","cookbook","copycat","corndog","cowboy","cowgirl","crossbow",
  "cupboard","cupcake","cutback","cutoff","daybed","daybreak","daylight","dogleg",
  "doorbell","doorknob","doorstep","doorway","downbeat","downcast","downfall",
  "dragonfly","drumstick","dustpan","earplug","earring","earwax","eggnog","eggplant",
  "eyebrow","eyecup","eyelash","eyelid","fallout","fanboy","feedback","fingertip",
  "firefly","fireman","fishbowl","fishnet","flagship","flashback","flyby","foghorn",
  "foothold","footprint","footstep","forget","forgave","frostbite","gameplay",
  "godson","goldfish","goodwill","grandchild","grassland","graveyard","guesswork",
  "gumball","gundrop","gunshot","haircut","halfway","hallway","handbook","handmade",
  "hangover","hardball","hardwood","hatbox","haystack","headband","herself","himself",
  "hogwash","homeland","hotbed","hotdog","hotshot","hubcap","iceberg","icecap",
  "inbox","inkblot","inkwell","inland","inlay","input","insight","into","itself",
  "jawbone","jigsaw","keyhole","keynote","kickback","kneecap","landmark","layout",
  "lawsuit","letdown","lifetime","lineup","lipstick","livestock","lockjaw","lookout",
  "mainstream","makeup","mankind","markdown","midday","midway","misfit","moonlight",
  "myself","nearby","network","newborn","newscast","nightfall","nobody","notebook",
  "nothing","nutmeg","offset","online","onset","onto","outcome","outcry","outdone",
  "outfox","outgrew","outgrow","outgun","outlaw","outline","outlook","output",
  "outran","outrun","outwit","overlap","padlock","pancake","pathway","payback",
  "payoff","peanut","pickup","pigpen","pigtail","pinball","pinpoint","pinup",
  "pitfall","playback","playmate","plywood","potluck","pushback","pushup",
  "ragweed","railway","rainbow","raindrop","rosebud","rubdown","runway","sailboat",
  "sandbox","sawdust","seagull","setback","shipwreck","shortcut","showdown",
  "shutdown","sidewalk","signup","skyline","snowball","snowflake","softball",
  "someone","something","somewhat","standoff","starfish","stopwatch","subplot",
  "sunburn","sunlit","sunspot","sunset","sunrise","sunshine","teacup","textbook",
  "timeout","tiptop","tomcat","topknot","topsoil","tugboat","turnip","turnout",
  "typecast","undone","unfair","unfit","unlike","unlock","unrest","until","unto",
  "update","upgrade","uphill","uphold","upon","upshot","upstairs","upstream",
  "upturn","upwind","vineyard","walnut","wardrobe","weekend","wildcat","wildlife",
  "windfall","windmill","within","without","woodland","yourself",
]);

// Small AI word bank grouped by length for computer player
const AI_WORDS = {
  2:["am","an","at","be","by","do","go","he","if","in","is","it","me","my","no","of","on","or","so","to","up","us","we"],
  3:["ace","act","add","age","ago","aid","aim","air","all","and","ant","any","ape","arc","are","ark","arm","art","ash","ate","awe","axe","bad","bag","ban","bar","bat","bay","bed","bet","bid","big","bin","bit","bow","box","boy","bud","bug","bun","bus","but","buy","cab","cam","can","cap","car","cat","cop","cow","cry","cub","cup","cur","cut","dab","dad","dam","day","den","dew","did","dig","dim","dip","dog","dot","dry","dub","dud","due","dug","dun","duo","dye","ear","eat","eel","egg","ego","elm","emu","end","era","eve","ewe","eye","fan","far","fat","fax","fed","fee","few","fig","fin","fir","fit","fix","fly","fob","foe","fog","for","fox","fry","fun","fur","gag","gap","gas","gay","gel","gem","get","gig","gin","gnu","god","got","gum","gun","gut","guy","gym","had","ham","has","hat","hay","hen","her","hew","hid","him","hip","his","hit","hob","hog","hop","hot","how","hub","hue","hug","hum","hut","ice","icy","ill","imp","ink","inn","ion","ire","irk","its","ivy","jab","jag","jam","jar","jaw","jay","jet","jig","job","jog","jot","joy","jug","jut","keg","ken","key","kid","kin","kit","lab","lac","lad","lag","lap","law","lay","lea","led","leg","let","lid","lie","lip","lit","log","lot","low","lug","mad","man","map","mar","mat","maw","may","men","met","mid","mix","mob","mod","mop","mow","mud","mug","mum","nab","nag","nap","net","new","nil","nip","nit","nod","nor","not","now","nun","nut","oak","oar","oat","odd","ode","off","oft","ohm","oil","old","one","opt","orb","ore","our","out","owe","owl","own","pad","pal","pan","pap","par","pat","paw","pay","pea","peg","pen","pep","per","pet","pew","pie","pig","pin","pit","ply","pod","pop","pot","pow","pro","pry","pub","pug","pun","pup","pus","put","quo","rag","ram","ran","rap","rat","raw","ray","red","ref","rib","rid","rig","rim","rip","rob","rod","roe","rot","row","rub","rug","rum","run","rut","rye","sac","sad","sag","sap","sat","saw","say","sea","set","sew","she","shy","sin","sip","sir","sis","sit","six","ski","sky","sly","sob","sod","son","sop","sot","sow","soy","spa","spy","sty","sub","sue","sum","sun","sup","tab","tad","tag","tan","tap","tar","tat","tax","tea","ten","the","thy","tic","tie","tin","tip","toe","ton","too","top","tot","tow","toy","try","tub","tug","two","urn","use","van","vat","vet","via","vie","vim","vow","wad","wag","war","was","wax","way","web","wed","wet","who","why","wig","win","wit","woe","wok","won","woo","wow","yak","yam","yap","yaw","yea","yes","yet","yew","you","zap","zed","zen","zig","zip","zoo"],
  4:["able","ache","acme","acne","acre","aged","also","arch","area","army","auto","avid","away","axle","back","bade","bait","bake","bald","bale","ball","band","bane","bang","bank","bare","bark","barn","base","bath","bead","beak","beam","bean","bear","beat","been","beer","bell","belt","bend","bent","best","bias","bike","bill","bind","bird","bite","blow","blue","blur","boar","boat","body","bold","bolt","bomb","bond","bone","book","boom","boot","bore","born","boss","both","bout","bowl","burn","bush","bust","busy","cafe","cage","cake","calf","call","calm","came","camp","cane","cape","card","care","cart","case","cash","cast","cave","cell","chat","chef","chin","chip","chop","city","clad","claim","clam","clan","clap","claw","clay","clip","clock","clot","club","clue","coal","coat","code","coil","coin","cold","colt","comb","come","cone","cook","cool","cope","copy","cord","core","cork","corn","cost","cosy","coup","cove","crab","crew","crop","crow","cube","cult","curb","cure","curl","cute","daft","dale","dame","damp","dare","dark","darn","dart","dash","data","date","dawn","days","dead","deaf","deal","dear","debt","deck","deed","deem","deep","deer","demo","dent","deny","desk","dial","dice","diet","dime","dine","dire","dirt","disc","dish","dock","does","dome","done","doom","door","dose","dots","down","doze","drab","drag","draw","drew","drip","drop","drum","dual","duck","duel","duet","duke","dull","dumb","dump","dune","dunk","dusk","dust","duty","dyed","each","earl","earn","ease","east","easy","edge","edit","else","emit","ends","epic","even","ever","evil","exam","exit","eyed","eyes","face","fact","fade","fail","fair","fake","fall","fame","fang","fare","farm","fast","fate","fawn","fear","feat","feed","feel","feet","fell","felt","fern","fest","file","fill","film","find","fine","fire","firm","fish","fist","five","flag","flap","flat","flaw","flea","fled","flew","flip","flit","flog","flow","foam","foil","fold","folk","fond","font","food","fool","foot","ford","fore","fork","form","fort","foul","four","free","frog","from","fuel","full","fume","fund","fury","fuse","fuss","gain","gait","gale","gall","game","gang","gape","garb","gate","gave","gaze","gear","germ","gift","gild","gill","girl","gist","give","glad","glen","glow","glue","glum","glut","gnat","gnaw","goat","goes","gold","golf","gone","good","gore","gory","gown","grab","grim","grin","grip","grit","grow","gulf","gust","guts","hack","hail","hair","hale","half","hall","halt","hand","hang","hare","harm","harp","hash","haste","hate","haul","have","haze","hazy","head","heal","heap","hear","heat","heed","heel","heir","held","hell","help","herb","herd","here","hero","hide","high","hike","hill","hilt","hind","hint","hire","hold","hole","holy","home","hone","hood","hoof","hook","hope","horn","hose","host","hour","howl","huff","huge","hull","hump","hung","hunt","hurl","hurt","hush","hymn","icon","idea","idle","inch","info","into","iron","isle","item","jack","jade","jail","jamb","jars","jazz","jest","jilt","jobs","jock","join","joke","jolt","jots","jowl","joys","judo","jugs","jump","June","July","junk","jury","just","keen","keep","kelp","kemp","kept","keys","kick","kids","kill","kind","king","kiss","kite","knack","knee","knew","knit","knob","knot","know","lace","lack","lacy","laid","lair","lake","lamb","lame","lamp","land","lane","lard","lark","lash","lass","last","late","lawn","lazy","lead","leaf","leak","lean","leap","left","lend","lens","lent","less","lest","levy","lick","lied","lieu","life","lift","like","limb","lime","limp","line","link","lint","lion","lips","list","live","load","loaf","loam","loan","lock","loft","logo","lone","long","look","loop","loot","lord","lore","lose","loss","lost","loud","love","luck","lull","lump","lung","lure","lurk","lush","lust","lynx","mace","made","maid","mail","main","make","male","mall","malt","mane","many","maps","mare","mark","mars","mash","mask","mass","mast","mate","math","maze","mead","meal","mean","meat","meek","meet","meld","melt","memo","mend","menu","mesh","mess","mice","mild","mile","milk","mill","mime","mind","mine","mint","mire","miss","mist","mite","moan","moat","mock","mode","moist","mold","mole","molt","monk","mood","moon","moor","moot","more","morn","moss","most","moth","move","much","muck","muff","mule","mull","mumps","murk","muse","mush","musk","must","mute","myth","nail","name","nape","navy","near","neat","neck","need","nest","nets","news","next","nice","nick","nine","node","none","nook","noon","norm","nose","note","noun","nude","null","numb","oath","obey","odds","odor","oils","oily","okay","omen","omit","once","ones","only","onto","ooze","opal","open","opts","oral","orca","orgy","oust","oven","over","owed","oxen","pace","pack","pact","page","paid","pail","pain","pair","pale","palm","pane","pang","pant","park","part","pass","past","path","pave","pawn","peak","peal","pear","peat","peck","peel","peer","pelt","pend","pens","perk","perm","pest","pick","pier","pike","pile","pill","pine","pink","pins","pint","pipe","pity","plan","play","plea","plod","plot","plow","ploy","plug","plum","plus","pock","poem","poet","poke","pole","poll","polo","pomp","pond","pony","pool","poor","pope","pops","pore","pork","port","pose","posh","post","pour","pout","pray","prep","prey","prod","prop","prow","prude","prune","puck","pull","pulp","pump","punk","puns","pure","push","quiz","race","rack","raft","rage","rags","raid","rail","rain","rake","ramp","rams","rang","rank","rant","rare","rash","rasp","rate","rave","rays","read","real","ream","reap","rear","reef","reel","rein","rely","rend","rent","rest","rice","rich","ride","rift","rigs","rile","rill","rime","rims","rind","ring","rink","riot","ripe","rise","risk","rite","road","roam","roar","robe","rock","rode","role","roll","romp","roof","rook","room","root","rope","rose","rosy","rote","rout","rove","rubs","rude","rued","ruff","ruin","rule","rump","rune","rung","runs","runt","ruse","rush","rust","ruts","sack","safe","saga","sage","said","sail","sake","sale","salt","same","sand","sane","sang","sank","sash","save","says","scab","scam","scan","scar","seal","seam","sear","seas","seat","sect","seed","seek","seem","seen","self","sell","semi","send","sent","sept","sere","serf","sewn","shed","shim","shin","ship","shod","shoe","shook","shop","shore","shot","show","shred","shrub","shrug","shut","sick","side","sift","sigh","sign","silk","sill","silt","since","sing","sink","sire","site","size","skit","slab","slag","slam","slap","slat","slaw","sled","slew","slid","slim","slit","slob","slog","slop","slot","slow","slug","slum","slur","smack","smog","snap","snare","snip","snob","snore","snow","snub","snug","soak","soap","soar","sock","soda","sofa","soft","soil","sold","sole","some","song","soon","soot","sore","sort","soul","soup","sour","sown","span","spar","spat","sped","spew","spin","spit","spot","spry","spud","spur","stab","stag","star","stay","stem","step","stew","stir","stop","stub","stud","stun","sway","swim","swoop","tabs","tack","tact","tail","take","tale","talk","tall","tame","tang","tank","tape","taps","tarn","task","taxi","teak","teal","team","tear","tell","temp","tend","tens","tent","term","test","text","than","that","them","then","they","thin","this","tick","tide","tidy","tied","tier","tile","till","tilt","time","tine","tiny","tips","tire","toad","toil","told","toll","tomb","tome","tone","tons","took","tool","tops","tore","torn","toss","tour","town","toys","trap","tray","tree","trek","trim","trio","trip","trod","trot","true","tuck","tuft","tuna","tune","turf","turn","tusk","twin","type","ugly","undo","unit","unto","upon","urge","used","user","uses","vague","vain","vale","vane","vary","vase","vast","veal","veer","veil","vein","vent","verb","very","vest","veto","vial","vice","vied","view","vile","vine","visa","void","vole","volt","vote","wade","wage","wail","wait","wake","walk","wall","wand","want","ward","warm","warn","warp","wart","wary","wash","wasp","wave","wavy","waxy","ways","weak","wean","wear","weed","week","weep","weld","well","went","were","west","what","when","whom","wick","wide","wife","wild","will","wilt","wily","wimp","wind","wine","wing","wink","wipe","wire","wise","wish","wisp","with","wits","woke","wolf","womb","wood","wool","word","wore","work","worm","worn","wove","wrap","wren","yank","yard","yarn","year","yell","yelp","yoga","yoke","your","zeal","zero","zest","zinc","zone","zoom"],
  5:["abort","about","above","abuse","acorn","acute","adapt","admit","adopt","adult","after","again","agent","agile","aging","agree","alarm","album","alert","alien","align","alike","alive","alley","allot","allow","alloy","alone","along","alter","ample","angel","anger","angle","angry","ankle","annex","antic","anvil","apart","apple","apply","arena","argue","arise","armor","aroma","aside","asset","atlas","attic","avail","avoid","awake","award","aware","awful","bacon","badge","badly","bagel","basic","basin","basis","batch","beach","beast","begin","being","below","bench","berry","bible","black","blade","blame","bland","blank","blast","blaze","bleak","bleat","bleed","blend","bless","blimp","blind","bliss","block","bloke","blond","blood","bloom","blown","bluff","blunt","blurt","blush","board","boast","bonus","boost","booth","bound","brace","brain","brand","brave","bread","break","breed","brick","bride","brief","bring","brink","brisk","broad","broke","brook","broth","brown","brush","budge","build","built","bulge","bunch","burst","buyer","cabin","cable","camel","canal","candy","carat","cargo","carry","carve","catch","cater","cause","cedar","chain","chair","chalk","champ","chant","chaos","charm","chart","chase","cheap","cheat","check","cheek","cheer","chest","chief","child","china","choir","choke","chord","chose","chunk","churn","cider","cigar","cinch","claim","clamp","clash","clasp","class","clean","clear","clerk","click","cliff","climb","cling","cloak","clock","clone","close","cloth","cloud","clown","coach","coast","cobra","comet","comic","coral","could","count","court","cover","crack","craft","crane","crash","crate","craze","crazy","creak","cream","crest","crisp","cross","crowd","crown","crude","cruel","crush","curve","cycle","daily","dance","debut","decay","decor","decoy","delay","delta","demon","dense","depot","depth","derby","devil","diary","dirty","dodge","donor","doubt","dough","draft","drain","drape","drawn","dream","dress","dried","drift","drill","drink","drive","droit","drone","drool","drove","drums","drunk","dryer","dunce","dusty","dwarf","dwell","dying","eager","eagle","early","earth","easel","eight","elder","elect","elite","ember","empty","endow","enemy","enjoy","enter","entry","envoy","equal","equip","erode","error","erupt","essay","ethic","evade","event","every","exact","exalt","excel","exile","exist","extra","exult","fable","facet","faint","fairy","faith","false","fancy","fault","feast","ferry","fetch","fever","fewer","fiber","field","fiend","fifth","fifty","fight","final","first","flame","flank","flare","flash","flask","flesh","flick","fling","flint","float","flock","flood","floor","flora","flour","flown","fluid","fluke","flung","flunk","flush","flute","focal","foggy","force","forge","forth","forty","forum","found","frame","frank","fraud","fresh","front","frost","froze","fruit","fully","fungi","gamma","gauge","gavel","ghost","giant","given","gland","glare","glass","gleam","glide","globe","gloom","glory","gloss","glove","gorge","gouge","grace","grade","grain","grand","grant","grape","graph","grasp","grass","grate","grave","gravy","great","greed","green","greet","grief","grill","grind","gripe","groan","groin","groom","grope","gross","group","grove","growl","grown","guard","guess","guest","guide","guild","guilt","guise","gulch","gully","gumbo","gummy","gusto","gusty","habit","harsh","haunt","haven","heart","heave","heavy","hedge","heist","hence","herbs","hinge","hippo","hitch","hobby","hoist","holly","homer","honor","horsy","hotel","hound","house","human","humid","humor","hurry","hyena","ideal","image","imply","inbox","index","inner","input","inter","intro","ivory","jewel","joker","jolly","judge","juice","juicy","jumbo","jumpy","kazoo","knack","knead","kneel","knelt","knife","knock","known","label","lance","large","laser","latch","later","laugh","layer","learn","lease","least","ledge","legal","lemon","level","lever","light","lilac","linen","liner","lingo","liver","llama","local","lodge","lofty","logic","lucid","lucky","lunar","lunch","lunge","lusty","lyric","macro","magic","major","maker","manor","maple","march","marsh","match","mayor","mealy","media","mercy","merge","merit","metal","meter","might","mince","minor","mirth","model","mogul","moist","money","month","moose","moral","morph","motor","motto","mound","mount","mourn","mouse","mouth","movie","mover","much","muddy","mulch","mural","murky","music","naive","nerve","never","noble","noise","north","notch","noted","novel","nudge","nurse","nylon","occur","ocean","oddly","offal","offer","often","olive","onion","onset","opera","orbit","order","organ","other","ought","outer","outgo","owned","owner","oxide","ozone","paint","panel","panic","paste","patch","pause","peach","pearl","pecan","pedal","penal","penny","perch","peril","perky","phase","phone","photo","piano","piece","pilot","pinch","pitch","pixel","pizza","place","plaid","plain","plait","plane","plank","plant","plate","plaza","plead","pleat","plier","pluck","plumb","plume","plump","plunk","point","polar","pooch","posed","pouch","pound","power","press","price","pride","prime","prince","print","prior","prism","probe","prone","proof","prose","proud","prove","prude","prune","psalm","pulse","punch","pupil","purge","purse","pushy","quail","qualm","queen","query","quest","queue","quick","quiet","quilt","quirk","quite","quota","quote","racer","radar","radio","rally","ranch","range","rapid","ratio","razor","reach","react","ready","realm","rebel","rebus","recap","refer","reign","relax","relay","renal","renew","repay","repel","reply","resin","retro","rider","ridge","rifle","rigid","rinse","ripen","risen","risky","rival","river","rivet","roast","robin","robot","rocky","rogue","round","route","rover","royal","rugby","ruler","rumba","rumor","rural","rusty","sadly","saint","salad","salon","salsa","sandy","sauce","sauna","savor","scale","scald","scalp","scant","scare","scene","scent","scope","score","scout","scowl","scrap","screw","scrub","sedan","seize","sense","serve","seven","sever","shade","shaft","shake","shame","shape","share","shark","sharp","shave","shawl","shear","sheen","sheep","sheer","sheet","shelf","shell","shift","shine","shirt","shock","shore","short","shout","shove","shown","shrub","shrug","sight","since","sixth","sixty","sized","skate","skill","skimp","skull","skunk","slate","slave","sleek","sleep","sleet","slice","slide","slime","slimy","slope","sloth","smart","smash","smell","smile","smirk","smith","smoke","snack","snake","snare","sneak","snore","solve","sonic","sorry","south","space","spare","spark","spawn","speak","spear","speed","spend","spent","spice","spicy","spill","spine","spoke","spoon","sport","spray","squad","stack","staff","stage","stain","stair","stake","stale","stalk","stall","stamp","stand","stank","stare","stark","start","state","stave","steak","steal","steam","steel","steep","steer","stern","stick","stiff","still","sting","stink","stint","stock","stoic","stoke","stole","stomp","stone","stood","stool","stoop","store","stork","storm","story","stout","stove","strap","straw","stray","strip","stuck","study","stuff","stump","stung","stunk","style","sugar","suite","sunny","super","surge","swamp","swarm","swear","sweat","sweep","sweet","swept","swift","swing","swirl","swoop","sword","swore","sworn","swung","syrup","table","talon","tango","taste","taunt","teach","tears","tease","tempo","tempt","tense","terms","thank","theft","their","theme","there","thick","thief","thigh","thing","think","third","thorn","those","three","threw","throw","thud","thumb","tiger","tight","timer","tinge","tired","titan","title","toast","today","token","topic","total","touch","tough","tower","towel","toxic","trace","track","trade","trail","train","trait","trash","tread","treat","treks","trend","trial","tribe","trick","tried","tripe","troll","troop","trout","truck","truly","trunk","trust","truth","tumor","tuned","tuner","ultra","uncle","under","undue","unity","untie","until","upper","upset","urban","usage","usher","usual","utter","valet","valid","valor","value","valve","vapor","vault","verge","verse","vigor","vinyl","viola","viper","virus","visit","visor","vital","vivid","vocal","vodka","vogue","voice","voter","vouch","vowel","vulva","wafer","wagon","waist","watch","water","weary","weave","wedge","weigh","weird","wheat","wheel","where","which","while","whine","whirl","white","whole","whose","widen","width","wield","windy","witch","woman","world","worry","worse","worst","worth","would","wound","wrack","wrath","wreak","wreck","wrist","write","wrong","wrote","yacht","yearn","yield","young","yours","youth","zebra","zesty"],
  6:["abrupt","absorb","accent","accept","access","across","action","active","actual","adjust","admire","affirm","afford","agency","agenda","alight","allied","almost","always","amazed","amount","anchor","animal","annual","answer","anyone","anyway","appeal","appear","archer","around","arrest","arrive","artist","asleep","aspect","assert","assign","assist","assume","attach","attack","attend","august","author","autumn","backer","banner","barely","barley","barrel","basket","batten","battle","beacon","beauty","become","before","behave","behind","belong","beside","better","beyond","bishop","bitter","blanch","blazer","blithe","bolster","bonnet","border","borrow","botany","bottom","bounce","breach","breath","breeze","bridge","bright","broken","bronze","browse","bruise","bubble","bucket","budget","buffer","bundle","burden","bureau","burner","button","bypass","calmly","camera","campus","cancel","candle","canyon","carbon","carpet","carrot","castle","casual","caught","causal","center","cereal","chance","change","chapel","charge","cherry","choice","chosen","chrome","church","circle","circus","citrus","clever","client","climax","clique","closed","closet","clumsy","clutter","cobalt","coffee","collar","colony","column","combat","comedy","coming","commit","common","compel","comply","convex","convoy","cookie","cooler","copper","corner","costly","cotton","couple","coupon","course","cousin","cradle","create","credit","crisis","critic","cruise","custom","cyborg","damage","dampen","dancer","danger","darkly","dazzle","debate","debris","decade","decent","decide","decode","decree","defeat","defend","define","deftly","degree","delete","deluxe","demand","denial","depart","depend","deploy","deputy","derive","desert","design","desire","detail","detect","devote","device","devour","dialog","differ","digest","dinner","direct","disarm","discus","dismal","dismay","dispel","divert","divide","domain","donkey","double","drains","dragon","drawer","driven","driver","duster","earthy","easily","eating","eighty","either","elapse","eldest","eleven","emerge","empire","employ","enable","encamp","encore","endure","energy","engage","engine","enjoy","enough","ensure","entire","entity","errand","escape","estate","esteem","evolve","exceed","except","excise","excuse","exempt","exhale","expand","expect","expert","export","expose","extend","extent","fabric","facial","factor","fairly","family","famine","famous","farmer","fathom","fatten","faucet","feline","fellow","female","fender","ferret","fickle","fierce","figure","filled","filter","finale","finder","finger","fiscal","flawed","flavor","flight","flinch","floral","flower","fluffy","flurry","foliage","follow","fondle","footer","forced","forged","forget","formal","format","former","fossil","foster","fought","fourth","frozen","frugal","fumble","funded","furrow","future","gadget","gained","galaxy","gallon","gambit","gamble","gentle","gently","gifted","ginger","glance","glimpse","global","glossy","golden","gospel","gossip","govern","gravel","greedy","groove","ground","growth","guided","guilty","guitar","gutter","guzzle","hamlet","hammer","hamper","handle","hangar","happen","harbor","hardly","hasty","haunch","hazard","header","health","heaven","height","helmet","helpful","herbal","heroic","hidden","hinder","hockey","hollow","honest","hooked","horror","hosted","hourly","housed","huddle","humble","hunter","hurdle","hustle","hybrid","ignite","ignore","impact","import","impose","impure","income","indoor","infant","inform","injure","inmate","insect","inside","insist","insult","intact","intend","intern","invade","invent","invest","invite","inward","island","itself","jaunty","jargon","jersey","jiggle","jingle","jockey","jostle","jumble","jungle","junior","kernel","kettle","kidney","kindle","kindly","knight","lament","laptop","lately","launch","lavish","lawyer","layout","league","lender","length","lessen","lesson","letter","likely","linger","liquid","listen","litter","little","lively","lizard","locker","lonely","longer","loosen","lovely","lumber","luxury","magnet","malice","manage","manner","manual","marble","margin","market","marvel","master","matter","meadow","medium","melody","member","memoir","memory","mental","mentor","method","mighty","miller","mingle","minute","mirror","modest","modify","molten","moment","monkey","mortal","mostly","mother","motion","motive","muffin","mumble","muscle","museum","muster","mutton","mutual","muzzle","myself","narrow","native","nature","nearby","nearly","neatly","needed","needle","nickel","nimble","nobody","noodle","normal","notary","notice","nought","number","object","obtain","occupy","offend","office","offset","online","opener","oppose","option","oracle","orange","ordeal","orient","origin","orphan","outage","outbid","outcry","outfit","outlaw","outlet","output","outrun","outwit","oyster","pacify","packet","paddle","palace","palate","parcel","parent","parish","parrot","partly","pastry","patrol","patron","patter","paving","peachy","pebble","pencil","people","pepper","period","permit","person","phrase","picket","pierce","pillar","pirate","planet","plaque","plaster","played","player","please","pledge","plenty","pliers","plunge","pocket","poetry","poison","policy","polish","polite","pollen","poorly","poplar","portal","poster","potato","potion","potter","poultry","powder","prayer","prefer","prefix","pretty","prince","prison","profit","prompt","proper","propel","proven","public","puddle","pummel","punish","puppet","purely","purple","pursue","puzzle","quaint","qualms","quarry","quench","quilts","rabbit","racket","radish","ragged","raisin","ramble","random","ranger","ransom","rascal","rather","rating","ravage","really","reason","recall","recent","recipe","record","reduce","reform","refuge","regard","regime","region","regret","reject","relate","relics","relief","relish","reluct","remain","remedy","remote","remove","render","repair","repeat","report","rescue","resign","resist","resort","result","resume","retail","retain","retire","return","reveal","review","revolt","reward","ribbon","riddle","ripple","ritual","robust","rocket","roster","rotary","ruffle","ruling","rumble","rustic","rusted","sacred","saddle","safety","salmon","sample","sandal","savage","saving","saying","scarce","scenic","scheme","school","screen","scroll","search","season","second","secret","sector","secure","seldom","select","seller","senior","serial","series","settle","shadow","shanty","shield","signal","silent","silver","simple","simply","singer","single","sister","sketch","slight","slogan","sloped","slowly","smooth","snatch","soaked","socket","soften","solely","solemn","solved","sorrow","sought","source","speech","spirit","splash","spoken","sponge","spread","spring","sprout","square","squash","stable","stagger","stance","staple","status","steady","stench","stitch","stolen","strain","strand","strap","streak","stream","street","stress","strict","stride","strike","string","stripe","stroke","strong","struck","studio","submit","subtle","suburb","sudden","suffix","summer","summit","sunken","superb","supply","surely","survey","switch","symbol","syntax","system","tablet","tackle","tailor","talent","tangle","target","teapot","temple","tenant","tender","terror","thirst","thirty","thorny","though","thrive","throne","thrown","thrust","thwart","ticket","tickle","tiebreaker","timber","timely","tissue","toggle","tomato","tongue","toward","travel","treaty","tremor","tribal","triple","trophy","trowel","truant","trudge","tunnel","turkey","turtle","tuxedo","twelve","twenty","tycoon","typist","unable","unborn","undone","unfair","unfold","unhook","unique","united","unjust","unkind","unless","unlike","unlock","unpack","unpaid","unrest","unruly","unsafe","unseen","unsure","untidy","untold","unused","unveil","unwell","unwind","upbeat","update","upheld","uphold","upkeep","uppity","uproar","uproot","upshot","upturn","upward","urgent","usable","useful","utmost","vacant","vacuum","valley","vanish","vanity","varied","vastly","velvet","vendor","veneer","vessel","victim","viewer","vigour","violet","virtue","visual","vivify","volley","volume","voting","voyage","waffle","walker","wallet","walnut","wander","wanted","warden","warmly","warmth","wealth","weapon","weekly","weight","wicked","widely","willed","willow","window","winner","winter","wisdom","within","wonder","wooden","worker","worthy","wreath","writer","yearly","yellow","yogurt","zenith"],
};

const TOTAL_ROUNDS = 13;
const MAX_REROLLS = 2;

function getLetterValue(l) { return LETTER_VALUES[l] || 0; }
function sumLetterValues(ls) { return ls.reduce((s, l) => s + getLetterValue(l), 0); }
function shuffleArray(a) { const n=[...a]; for(let i=n.length-1;i>0;i--){const j=Math.floor(Math.random()*(i+1));[n[i],n[j]]=[n[j],n[i]];} return n; }

function wordToLetters(word) {
  const ls=[]; let i=0; const u=word.toUpperCase();
  while(i<u.length){if(u[i]==="Q"&&i+1<u.length&&u[i+1]==="U"){ls.push("Qu");i+=2;}else{ls.push(u[i]);i++;}} return ls;
}
function canFormWord(wl,avail){const p=[...avail];for(const l of wl){const i=p.indexOf(l);if(i===-1)return false;p.splice(i,1);}return true;}
function isPalindrome(w){const s=w.toLowerCase();return s.length>=3&&s===s.split("").reverse().join("");}
function hasDoubleLetters(w){const s=w.toLowerCase();const m={};for(const c of s)m[c]=(m[c]||0)+1;return Object.values(m).some(v=>v>=2);}
function hasRareLetter(wl){const r=new Set(["J","K","Qu","V","X","Z"]);return wl.some(l=>r.has(l));}
function allUniqueLetters(w){const s=w.toLowerCase();return new Set(s.split("")).size===s.length;}
function isCompoundWord(w){return COMPOUND_WORDS.has(w.toLowerCase());}

function rollAllDice(){return shuffleArray(DICE_CONFIG.map((f,i)=>({letter:f[Math.floor(Math.random()*6)],dieIndex:i,id:Math.random()})));}
function rerollSingleDie(d){const f=DICE_CONFIG[d.dieIndex];return{...d,letter:f[Math.floor(Math.random()*6)],id:Math.random()};}

function getCategoriesForDifficulty(diff) {
  const upper=[
    {id:"2letter",name:"2-Letter Word",section:"upper",req:"Exactly 2 letters",lengthReq:2,bonus:1},
    {id:"3letter",name:"3-Letter Word",section:"upper",req:"Exactly 3 letters",lengthReq:3,bonus:3},
    {id:"4letter",name:"4-Letter Word",section:"upper",req:"Exactly 4 letters",lengthReq:4,bonus:6},
    {id:"5letter",name:"5-Letter Word",section:"upper",req:"Exactly 5 letters",lengthReq:5,bonus:10},
    {id:"6letter",name:"6-Letter Word",section:"upper",req:"Exactly 6 letters",lengthReq:6,bonus:15},
  ];
  const lowerMap={
    easy:[{id:"free",name:"Free Word",section:"lower",type:"free"},{id:"noun",name:"Noun",section:"lower",type:"pos",pos:"noun"},{id:"verb",name:"Verb",section:"lower",type:"pos",pos:"verb"},{id:"adjective",name:"Adjective",section:"lower",type:"pos",pos:"adjective"},{id:"double",name:"Double Letters",section:"lower",type:"double"},{id:"rare",name:"Rare Letter",section:"lower",type:"rare"},{id:"palindrome",name:"Palindrome",section:"lower",type:"palindrome"},{id:"wordtzee",name:"Wordtzee!",section:"lower",type:"wordtzee"}],
    medium:[{id:"free",name:"Free Word",section:"lower",type:"free"},{id:"noun",name:"Noun",section:"lower",type:"pos",pos:"noun"},{id:"double",name:"Double Letters",section:"lower",type:"double"},{id:"unique",name:"All Unique",section:"lower",type:"unique"},{id:"rare",name:"Rare Letter",section:"lower",type:"rare"},{id:"twoforone",name:"Two-for-One",section:"lower",type:"twoforone"},{id:"palindrome",name:"Palindrome",section:"lower",type:"palindrome"},{id:"wordtzee",name:"Wordtzee!",section:"lower",type:"wordtzee"}],
    hard:[{id:"noun",name:"Noun",section:"lower",type:"pos",pos:"noun"},{id:"double",name:"Double Letters",section:"lower",type:"double"},{id:"unique",name:"All Unique",section:"lower",type:"unique"},{id:"rare",name:"Rare Letter",section:"lower",type:"rare"},{id:"twoforone",name:"Two-for-One",section:"lower",type:"twoforone"},{id:"compound",name:"Compound Word",section:"lower",type:"compound"},{id:"palindrome",name:"Palindrome",section:"lower",type:"palindrome"},{id:"wordtzee",name:"Wordtzee!",section:"lower",type:"wordtzee"}],
  };
  return [...upper,...(lowerMap[diff]||lowerMap.easy)];
}

function calcScore(word,wl,cat){
  const lv=sumLetterValues(wl);
  if(cat.section==="upper")return cat.bonus+lv;
  switch(cat.type){case "free":return lv;case "pos":return 2*lv;case "double":return 10+lv;case "unique":return 10+lv;case "rare":return 15+lv;case "palindrome":return 25+lv;case "compound":return 20+lv;case "wordtzee":return 50;case "twoforone":return 15+lv;default:return lv;}
}

function getScoreBreakdown(word,wl,cat){
  const lines=[];
  if(cat.section==="upper"){lines.push({label:`Length bonus (${cat.lengthReq}-letter)`,value:`+${cat.bonus}`});wl.forEach(l=>lines.push({label:`Letter: ${l}`,value:`+${getLetterValue(l)}`}));}
  else if(cat.type==="wordtzee"){lines.push({label:"Wordtzee! flat bonus",value:"50"});}
  else if(cat.type==="twoforone"){lines.push({label:"Two-for-One bonus",value:"+15"});wl.forEach(l=>lines.push({label:`Letter: ${l}`,value:`+${getLetterValue(l)}`}));}
  else{const bm={free:0,pos:0,double:10,unique:10,rare:15,palindrome:25,compound:20};const bn={double:"Double Letters bonus",unique:"All Unique bonus",rare:"Rare Letter bonus",palindrome:"Palindrome bonus",compound:"Compound Word bonus"};const m=cat.type==="pos"?2:1;if(bm[cat.type])lines.push({label:bn[cat.type],value:`+${bm[cat.type]}`});if(m>1){lines.push({label:`${cat.name} (2× letters)`,value:""});wl.forEach(l=>lines.push({label:`Letter: ${l}`,value:`${getLetterValue(l)} × 2 = +${getLetterValue(l)*2}`}));}else{wl.forEach(l=>lines.push({label:`Letter: ${l}`,value:`+${getLetterValue(l)}`}));}}
  return lines;
}

function checkCategoryEligibility(word,wl,cat,pos,all){
  if(cat.section==="upper")return word.length===cat.lengthReq;
  switch(cat.type){case "free":return word.length>=2;case "pos":return pos.includes(cat.pos);case "double":return hasDoubleLetters(word);case "unique":return allUniqueLetters(word);case "rare":return hasRareLetter(wl);case "palindrome":return isPalindrome(word);case "compound":return isCompoundWord(word);case "wordtzee":return wl.length===7&&canFormWord(wl,all);case "twoforone":return true;default:return false;}
}

// Build offline dictionary from AI_WORDS
const VALID_WORDS_SET = new Set(Object.values(AI_WORDS).flat().map(w=>w.toLowerCase()));

// Common words the AI_WORDS list might miss — add extras
["curly","curls","wavy","dusty","rusty","foggy","bumpy","crispy","flaky","grumpy","handy","jazzy","lucky","messy","nerdy","picky","risky","salty","sandy","shady","shiny","silky","smoky","spicy","stony","tasty","tricky","windy","witty","woody","zesty",
"swings","brings","clings","flings","rings","sings","stings","strings","things","wings",
"waved","saved","caved","paved","braved","craved","shaved","waving","saving","caving",
"waves","saves","caves","braves","craves","shaves",
"walked","talked","asked","jumped","pulled","pushed","called","turned","played","moved",
"walks","talks","asks","jumps","pulls","calls","turns","plays","moves",
"walking","talking","asking","jumping","pulling","pushing","calling","turning","playing","moving",
"bigger","faster","slower","longer","deeper","wider","higher","lower","closer","darker",
"biggest","fastest","slowest","longest","deepest","widest","highest","lowest","closest","darkest",
"quickly","slowly","gently","softly","firmly","warmly","coldly","wildly","fairly","nearly",
"running","sitting","getting","letting","cutting","hitting","putting","winning","beginning",
"runs","sits","gets","lets","cuts","hits","puts","wins",
"tries","cries","flies","dries","dies","lies","ties","applies","replies","denies",
"tried","cried","dried","died","lied","tied","applied","replied","denied",
"trying","crying","flying","drying","dying","lying","tying",
"foxes","boxes","mixes","fixes","taxes","axes","waxes",
"churches","watches","matches","catches","teaches","reaches","beaches","peaches",
"babies","ladies","cities","bodies","stories","parties","copies","studies",
"knives","leaves","wolves","lives","wives","halves","shelves","thieves",
"children","women","teeth","feet","mice","geese","oxen","men",
"bought","brought","caught","fought","sought","taught","thought",
"broken","chosen","frozen","spoken","stolen","woven","written","driven","given","taken",
"began","became","blew","broke","chose","drew","drove","ate","fell","flew","forgot","froze","gave","grew","hid","knew","led","lost","paid","ran","rode","rose","sang","sat","saw","sent","shook","slid","sold","spoke","stood","stole","stuck","swam","swept","swore","swung","threw","told","took","wore","woke","won","wrote",
].forEach(w=>VALID_WORDS_SET.add(w.toLowerCase()));

// Suffix rules for stripping inflections to find root
const SUFFIX_RULES=[
  {s:"ies",r:"y"},{s:"ves",r:"fe"},{s:"ves",r:"f"},
  {s:"ses",r:"s"},{s:"zes",r:"z"},{s:"xes",r:"x"},
  {s:"ches",r:"ch"},{s:"shes",r:"sh"},
  {s:"ing",r:"e"},{s:"ing",r:""},
  {s:"ting",r:"t"},{s:"ning",r:"n"},{s:"ping",r:"p"},
  {s:"ging",r:"g"},{s:"bing",r:"b"},{s:"ming",r:"m"},{s:"ding",r:"d"},
  {s:"ling",r:"l"},{s:"ring",r:"r"},{s:"sing",r:"s"},
  {s:"ed",r:"e"},{s:"ed",r:""},
  {s:"ted",r:"t"},{s:"ned",r:"n"},{s:"ped",r:"p"},
  {s:"ged",r:"g"},{s:"bed",r:"b"},{s:"med",r:"m"},{s:"ded",r:"d"},
  {s:"led",r:"l"},{s:"red",r:"r"},{s:"sed",r:"s"},
  {s:"er",r:"e"},{s:"er",r:""},
  {s:"est",r:"e"},{s:"est",r:""},
  {s:"ier",r:"y"},{s:"iest",r:"y"},
  {s:"ly",r:""},{s:"ily",r:"y"},
  {s:"es",r:""},{s:"s",r:""},
  {s:"ness",r:""},{s:"ment",r:""},{s:"ment",r:"e"},
  {s:"ful",r:""},{s:"less",r:""},
  {s:"able",r:"e"},{s:"able",r:""},{s:"ible",r:""},
];

function isValidWord(word){
  const w=word.toLowerCase();
  if(VALID_WORDS_SET.has(w))return true;
  // Try suffix stripping
  for(const{s,r}of SUFFIX_RULES){
    if(w.endsWith(s)&&w.length>s.length){
      const stem=w.slice(0,w.length-s.length)+r;
      if(stem.length>=2&&VALID_WORDS_SET.has(stem))return true;
    }
  }
  return false;
}

// Simple POS guessing based on suffix patterns
function guessPOS(word){
  const w=word.toLowerCase();
  const pos=new Set();
  // Most words can be nouns
  pos.add("noun");
  // Verb patterns
  if(w.endsWith("ing")||w.endsWith("ed")||w.endsWith("es")||w.endsWith("s"))pos.add("verb");
  if(VALID_WORDS_SET.has(w)&&AI_WORDS[w.length>=2&&w.length<=6?w.length:0])pos.add("verb");
  // Adjective patterns
  if(w.endsWith("ly")||w.endsWith("ful")||w.endsWith("less")||w.endsWith("ous")||w.endsWith("ive")||w.endsWith("able")||w.endsWith("ible")||w.endsWith("al")||w.endsWith("ish")||w.endsWith("ic")||w.endsWith("y")||w.endsWith("er")||w.endsWith("est"))pos.add("adjective");
  return[...pos];
}

async function tryAPIValidation(word){
  try{
    const r=await fetch(`https://api.dictionaryapi.dev/api/v2/entries/en/${word.toLowerCase()}`);
    if(r.ok){const d=await r.json();return[...new Set(d.flatMap(e=>e.meanings.map(m=>m.partOfSpeech)))];}
  }catch{}
  try{
    const r=await fetch(`https://api.datamuse.com/words?sp=${word.toLowerCase()}&max=1&md=p`);
    if(r.ok){
      const d=await r.json();
      if(d.length>0&&d[0].word===word.toLowerCase()){
        const tags=d[0].tags||[];
        const posMap={"n":"noun","v":"verb","adj":"adjective","adv":"adverb"};
        return tags.filter(t=>posMap[t]).map(t=>posMap[t]);
      }
    }
  }catch{}
  return null;
}

async function validateWord(word){
  const w=word.toLowerCase();
  const offline=isValidWord(w);

  // Try APIs for better POS data (non-blocking bonus)
  const apiPOS=await tryAPIValidation(w);

  if(apiPOS){
    return{valid:true,pos:apiPOS};
  }
  if(offline){
    return{valid:true,pos:guessPOS(w)};
  }

  // Neither found it — try suffix stripping on API too
  for(const{s,r}of SUFFIX_RULES){
    if(w.endsWith(s)&&w.length>s.length){
      const stem=w.slice(0,w.length-s.length)+r;
      if(stem.length>=2){
        const stemAPI=await tryAPIValidation(stem);
        if(stemAPI)return{valid:true,pos:stemAPI};
      }
    }
  }

  return{valid:false,pos:[]};
}

// Computer AI: find best word from available letters
function computerFindWord(letters, categories, scores) {
  const available = letters.map(l => l.toUpperCase());
  const candidates = [];
  for (const len of [6,5,4,3,2]) {
    const words = AI_WORDS[len] || [];
    for (const w of words) {
      const wl = wordToLetters(w);
      if (canFormWord(wl, available)) candidates.push({ word: w, letters: wl, len });
    }
  }
  if (candidates.length === 0) return null;
  // Score each candidate against open categories
  let best = null; let bestScore = -1;
  for (const c of candidates) {
    for (const cat of categories) {
      if (scores[cat.id] !== undefined) continue;
      if (cat.type === "twoforone") continue;
      // Simple eligibility check (no POS for AI - just skip POS cats or guess)
      let eligible = false;
      if (cat.section === "upper") eligible = c.word.length === cat.lengthReq;
      else {
        switch(cat.type) {
          case "free": eligible = c.word.length >= 2; break;
          case "pos": eligible = true; break; // AI assumes any word might qualify
          case "double": eligible = hasDoubleLetters(c.word); break;
          case "unique": eligible = allUniqueLetters(c.word); break;
          case "rare": eligible = hasRareLetter(c.letters); break;
          case "compound": eligible = isCompoundWord(c.word); break;
          case "palindrome": eligible = isPalindrome(c.word); break;
          case "wordtzee": eligible = c.letters.length === 7; break;
        }
      }
      if (eligible) {
        const sc = calcScore(c.word, c.letters, cat);
        if (sc > bestScore) { bestScore = sc; best = { ...c, catId: cat.id, score: sc }; }
      }
    }
  }
  return best;
}

// ═══════════════════════════════════════════
// THEME
// ═══════════════════════════════════════════
const T={bg:"linear-gradient(160deg, #0c0e1a 0%, #151932 40%, #1a1f42 70%, #0f1225 100%)",accent:"#e8734a",accentGlow:"rgba(232,115,74,0.25)",accentGrad:"linear-gradient(135deg, #e8734a, #d4543a)",gold:"#f0c040",success:"#56d89c",successBg:"rgba(86,216,156,0.1)",successBorder:"rgba(86,216,156,0.3)",danger:"#e85c6a",text:"#e8e4f0",textDim:"rgba(232,228,240,0.5)",textFaint:"rgba(232,228,240,0.25)",dieBg:"linear-gradient(145deg, #f0eaff, #ddd6f0)",dieKeptBg:"linear-gradient(145deg, #fff5ea, #ffe8cc)",dieText:"#1a1530",easy:"#56d89c",medium:"#f0c040",hard:"#e85c6a"};

// ═══════════════════════════════════════════
// COMPONENTS
// ═══════════════════════════════════════════

function ScoreTooltip({breakdown,total,children}){
  const[show,setShow]=useState(false);const[pos,setPos]=useState({x:0,y:0});
  if(!breakdown||breakdown.length===0)return children;
  return(<div onMouseEnter={e=>{const r=e.currentTarget.getBoundingClientRect();setPos({x:r.left,y:r.top});setShow(true);}} onMouseLeave={()=>setShow(false)} style={{position:"relative",display:"inline-block"}}>{children}{show&&(<div style={{position:"fixed",left:Math.min((typeof window!=='undefined'?window.innerWidth:400)-220,Math.max(8,pos.x-100)),top:Math.max(8,pos.y-10),transform:"translateY(-100%)",background:"#1a1e45",border:"1px solid rgba(232,115,74,0.4)",borderRadius:10,padding:"10px 14px",zIndex:999,minWidth:170,maxWidth:240,boxShadow:"0 8px 28px rgba(0,0,0,0.85), 0 0 0 1px rgba(0,0,0,0.5)",pointerEvents:"none"}}><div style={{fontSize:9,fontWeight:700,textTransform:"uppercase",letterSpacing:2,color:T.accent,marginBottom:6}}>Score Breakdown</div>{breakdown.map((b,i)=>(<div key={i} style={{display:"flex",justifyContent:"space-between",fontSize:11,padding:"2px 0",color:T.text,opacity:1,gap:12}}><span>{b.label}</span><span style={{fontWeight:700,whiteSpace:"nowrap"}}>{b.value}</span></div>))}<div style={{borderTop:`1px solid ${T.textFaint}`,marginTop:5,paddingTop:5,display:"flex",justifyContent:"space-between",fontSize:13,fontWeight:800,color:T.accent}}><span>Total</span><span>{total}</span></div></div>)}</div>);
}

function ConfirmModal({message,onConfirm,onCancel}){
  return(<div style={{position:"fixed",inset:0,background:"rgba(0,0,0,0.7)",display:"flex",alignItems:"center",justifyContent:"center",zIndex:1000,padding:20}}><div style={{background:"linear-gradient(145deg, #1e2248, #252a52)",borderRadius:16,padding:24,maxWidth:360,width:"100%",border:"1px solid rgba(232,115,74,0.2)",boxShadow:"0 12px 40px rgba(0,0,0,0.5)"}}><div style={{fontSize:14,color:T.text,marginBottom:20,lineHeight:1.6}}>{message}</div><div style={{display:"flex",gap:10}}><button onClick={onCancel} style={{flex:1,padding:"10px",borderRadius:10,border:`1px solid ${T.textFaint}`,background:"transparent",color:T.text,fontFamily:"'Outfit',sans-serif",fontSize:14,fontWeight:600,cursor:"pointer"}}>Cancel</button><button onClick={onConfirm} style={{flex:1,padding:"10px",borderRadius:10,border:"none",background:T.accentGrad,color:"#fff",fontFamily:"'Outfit',sans-serif",fontSize:14,fontWeight:700,cursor:"pointer"}}>Confirm</button></div></div></div>);
}

function InstructionsModal({onClose}){
  const sectionStyle={marginBottom:16};
  const headingStyle={fontSize:14,fontWeight:700,color:T.accent,marginBottom:4,fontFamily:"'Righteous',cursive",letterSpacing:1};
  const textStyle={fontSize:12,color:T.text,lineHeight:1.6,margin:0};
  return(
    <div style={{position:"fixed",inset:0,background:"rgba(0,0,0,0.75)",display:"flex",alignItems:"center",justifyContent:"center",zIndex:1100,padding:16}} onClick={onClose}>
      <div onClick={e=>e.stopPropagation()} style={{background:"linear-gradient(145deg, #1e2248, #252a52)",borderRadius:16,padding:"24px 20px",maxWidth:400,width:"100%",border:"1px solid rgba(232,115,74,0.2)",boxShadow:"0 12px 40px rgba(0,0,0,0.5)",maxHeight:"85vh",display:"flex",flexDirection:"column"}}>
        <h2 style={{fontFamily:"'Righteous',cursive",fontSize:24,background:"linear-gradient(135deg, #e8734a, #f0c040)",WebkitBackgroundClip:"text",WebkitTextFillColor:"transparent",margin:"0 0 16px",textAlign:"center",letterSpacing:2}}>HOW TO PLAY</h2>
        <div style={{overflowY:"auto",flex:1,paddingRight:4}}>
          <div style={sectionStyle}>
            <div style={headingStyle}>Goal</div>
            <p style={textStyle}>Fill all 13 scoring categories to maximize your total score. Each category can only be used once!</p>
          </div>
          <div style={sectionStyle}>
            <div style={headingStyle}>Rolling</div>
            <p style={textStyle}>Roll 7 letter dice each turn. You get up to 2 rerolls — keep the letters you like before each reroll.</p>
          </div>
          <div style={sectionStyle}>
            <div style={headingStyle}>Keeping Letters</div>
            <p style={textStyle}>Tap dice to move them to your keep line. Drag to reorder your kept letters and spell a word.</p>
          </div>
          <div style={sectionStyle}>
            <div style={headingStyle}>Scoring</div>
            <p style={textStyle}>Check your word, then pick an eligible category to score it in. If nothing fits, you can scratch a category for 0 points.</p>
          </div>
          <div style={sectionStyle}>
            <div style={headingStyle}>Upper Section</div>
            <p style={textStyle}>Categories based on word length (3-letter, 4-letter, etc.) with length bonuses for longer words. Score 50+ points in the upper section to earn a +25 bonus!</p>
          </div>
          <div style={sectionStyle}>
            <div style={headingStyle}>Lower Section</div>
            <p style={textStyle}>Special categories like Noun, Verb, Palindrome, Rare Letter Word, and more. Each has unique scoring rules.</p>
          </div>
          <div style={sectionStyle}>
            <div style={headingStyle}>Letter Values</div>
            <p style={textStyle}>Common letters (E, A, R, S…) = 1 pt. Less common letters score more. Rare letters (J, X, Qu, Z) = 8–10 pts each!</p>
          </div>
        </div>
        <button onClick={onClose} style={{width:"100%",padding:"12px",borderRadius:10,border:"none",background:T.accentGrad,color:"#fff",fontFamily:"'Righteous',cursive",fontSize:16,letterSpacing:2,cursor:"pointer",marginTop:16,boxShadow:`0 4px 16px ${T.accentGlow}`}}>Got It!</button>
      </div>
    </div>
  );
}

// Die component
function Die({die,isKept,onClick,onDragStart,size=52}){
  const fs=die.letter==="Qu"?size*0.3:size*0.38;
  return(<div onClick={onClick} onPointerDown={onDragStart}
    style={{width:size,height:size,borderRadius:10,border:isKept?`2px solid ${T.accent}`:"2px solid transparent",
      background:isKept?T.dieKeptBg:T.dieBg,color:T.dieText,
      fontFamily:"'Outfit',sans-serif",fontSize:fs,fontWeight:800,cursor:"pointer",
      position:"relative",display:"flex",alignItems:"center",justifyContent:"center",flexShrink:0,
      boxShadow:isKept?`0 0 12px ${T.accentGlow}, 0 2px 6px rgba(0,0,0,0.3)`:"0 2px 8px rgba(0,0,0,0.3), inset 0 1px 0 rgba(255,255,255,0.5)",
      transition:"transform 0.1s",touchAction:"none",userSelect:"none",
    }}>{die.letter}
    <span style={{position:"absolute",bottom:1,right:2,fontSize:12,opacity:0.7,fontWeight:800}}>{getLetterValue(die.letter)}</span>
  </div>);
}

// ═══════════════════════════════════════════
// 3D DIE (for roll arena)
// ═══════════════════════════════════════════
function Die3D({die,size=54,spinning,tiltX=0,tiltY=0}){
  const half=size/2;
  const fs=die.letter==="Qu"?size*0.28:size*0.36;
  const rnd=useRef(Array.from({length:5},()=>"AEIOULNRSTDHCM"[Math.floor(Math.random()*14)]));
  const textColor="#1a1d3a";
  const pad=6; // oversize faces to cover corner seams
  const faceSize=size+pad;
  const off=-(pad/2);
  const rad=size*0.06;
  const fb={position:"absolute",width:faceSize,height:faceSize,left:off,top:off,display:"flex",alignItems:"center",justifyContent:"center",
    backfaceVisibility:"hidden",borderRadius:rad,fontFamily:"'Outfit',sans-serif",fontSize:fs,fontWeight:800,color:textColor};
  return(
    <div style={{width:size,height:size,perspective:600}}>
      <div style={{width:size,height:size,position:"relative",transformStyle:"preserve-3d",
        background:"#ddd5c5",borderRadius:0,
        animation:spinning?"cubeRoll 0.75s ease-out both":"none",
        transform:spinning?undefined:`rotateX(${tiltX}deg) rotateY(${tiltY}deg)`,
        transition:spinning?"none":"transform 0.5s ease-out"}}>
        {/* Front */}
        <div style={{...fb,background:"linear-gradient(145deg, #f5f0e8, #e8e0d0)",transform:`translateZ(${half}px)`,
          boxShadow:"inset 0 1px 0 rgba(255,255,255,0.8), inset 0 -1px 2px rgba(0,0,0,0.08)"}}>
          {die.letter}
          <span style={{position:"absolute",bottom:1,right:2,fontSize:12,opacity:0.7,fontWeight:800,color:textColor}}>{getLetterValue(die.letter)}</span>
        </div>
        {/* Back */}
        <div style={{...fb,background:"linear-gradient(145deg, #e0d8c8, #d5ccb8)",transform:`rotateY(180deg) translateZ(${half}px)`}}>{rnd.current[0]}</div>
        {/* Right */}
        <div style={{...fb,background:"linear-gradient(145deg, #ddd5c5, #d0c8b5)",transform:`rotateY(90deg) translateZ(${half}px)`}}>{rnd.current[1]}</div>
        {/* Left */}
        <div style={{...fb,background:"linear-gradient(145deg, #ddd5c5, #d0c8b5)",transform:`rotateY(-90deg) translateZ(${half}px)`}}>{rnd.current[2]}</div>
        {/* Top */}
        <div style={{...fb,background:"linear-gradient(145deg, #eae2d5, #ddd5c5)",transform:`rotateX(90deg) translateZ(${half}px)`}}>{rnd.current[3]}</div>
        {/* Bottom */}
        <div style={{...fb,background:"linear-gradient(145deg, #c8c0b0, #bbb3a3)",transform:`rotateX(-90deg) translateZ(${half}px)`}}>{rnd.current[4]}</div>
      </div>
    </div>
  );
}

// ═══════════════════════════════════════════
// DRAGGABLE KEEP LINE
// ═══════════════════════════════════════════
function DraggableKeepLine({keepLine,setKeepLine,onUnkeep,clearValidation,onDropFromPool,incomingDrag}){
  const[dragIdx,setDragIdx]=useState(null);const[dragOverIdx,setDragOverIdx]=useState(null);
  const[pointerPos,setPointerPos]=useState(null);const containerRef=useRef(null);const slotRefs=useRef([]);
  const dragStart=useRef(null);const dragging=useRef(false);

  // All pointer events on container so they always fire
  function handlePointerDown(e){
    // Find which die was pressed
    const idx=findDieIndex(e.target);
    if(idx===null)return;
    dragStart.current={x:e.clientX,y:e.clientY,idx};
    dragging.current=false;
    e.currentTarget.setPointerCapture(e.pointerId);
  }

  function findDieIndex(el){
    while(el&&el!==containerRef.current){
      if(el.dataset&&el.dataset.keepidx!==undefined)return parseInt(el.dataset.keepidx);
      el=el.parentElement;
    }
    return null;
  }

  function handlePointerMove(e){
    if(!dragStart.current)return;
    const dx=e.clientX-dragStart.current.x,dy=e.clientY-dragStart.current.y;
    if(!dragging.current&&Math.sqrt(dx*dx+dy*dy)>8){
      dragging.current=true;
      setDragIdx(dragStart.current.idx);
    }
    if(dragging.current){
      setPointerPos({x:e.clientX,y:e.clientY});
      // Compute insert position from visible slot positions (excluding dragged)
      const visibleSlots=[];
      for(let i=0;i<keepLine.length;i++){
        if(i===dragStart.current.idx)continue;
        const el=slotRefs.current[i];
        if(el){const r=el.getBoundingClientRect();visibleSlots.push({origIdx:i,left:r.left,right:r.right,cx:r.left+r.width/2});}
      }
      let insertBefore=keepLine.length; // default: after everything
      for(let i=0;i<visibleSlots.length;i++){
        if(e.clientX<visibleSlots[i].cx){
          insertBefore=visibleSlots[i].origIdx;
          break;
        }
      }
      // Convert to the index in the original array where the item would be inserted
      // after removing the dragged item
      setDragOverIdx(insertBefore);
    }
  }

  function handlePointerUp(e){
    if(dragging.current&&dragIdx!==null&&dragOverIdx!==null){
      const from=dragIdx;
      let to=dragOverIdx;
      if(from!==to){
        setKeepLine(k=>{
          const n=[...k];
          const[item]=n.splice(from,1);
          const insertAt=to>from?to-1:to;
          n.splice(insertAt,0,item);
          return n;
        });
        clearValidation();
      }
    }else if(!dragging.current&&dragStart.current){
      const d=keepLine[dragStart.current.idx];
      if(d)onUnkeep(d);
    }
    setDragIdx(null);setDragOverIdx(null);setPointerPos(null);dragStart.current=null;dragging.current=false;
    try{containerRef.current?.releasePointerCapture(e.pointerId);}catch(ex){}
  }

  // Build visual list: remove dragged die, insert ghost at computed position
  function getDisplayList(){
    // Internal reorder drag
    if(dragIdx!==null&&dragOverIdx!==null){
      const items=[];
      const ghostDie=keepLine[dragIdx];
      let insertAt=dragOverIdx>dragIdx?dragOverIdx-1:dragOverIdx;
      let vi=0;
      for(let i=0;i<keepLine.length;i++){
        if(i===dragIdx)continue;
        if(vi===insertAt){items.push({die:ghostDie,isGhost:true,origIdx:dragIdx});vi++;}
        items.push({die:keepLine[i],isGhost:false,origIdx:i});
        vi++;
      }
      if(insertAt>=vi)items.push({die:ghostDie,isGhost:true,origIdx:dragIdx});
      return items;
    }
    // Incoming drag from roll pool — show ghost at insert position
    if(incomingDrag){
      const items=[];
      const idx=incomingDrag.insertIdx;
      for(let i=0;i<keepLine.length;i++){
        if(i===idx)items.push({die:incomingDrag.die,isGhost:true,origIdx:-1});
        items.push({die:keepLine[i],isGhost:false,origIdx:i});
      }
      if(idx>=keepLine.length)items.push({die:incomingDrag.die,isGhost:true,origIdx:-1});
      return items;
    }
    return keepLine.map((d,i)=>({die:d,isGhost:false,origIdx:i}));
  }

  const displayList=getDisplayList();

  return(
    <div ref={containerRef}
      onPointerDown={handlePointerDown} onPointerMove={handlePointerMove} onPointerUp={handlePointerUp}
      style={{display:"flex",flexWrap:"wrap",gap:8,justifyContent:"center",minHeight:64,padding:10,borderRadius:14,
        background:incomingDrag?`rgba(232,115,74,0.08)`:`rgba(232,115,74,0.03)`,border:incomingDrag?`1px dashed ${T.accent}60`:`1px dashed ${T.accent}25`,position:"relative",touchAction:"none",userSelect:"none",transition:"background 0.15s, border-color 0.15s"}}>
      {keepLine.length===0&&!incomingDrag&&(<div style={{color:T.textFaint,fontSize:11,alignSelf:"center",padding:8}}>Tap or drag dice here to build your word</div>)}
      {displayList.map((item,vi)=>{
        const{die:d,isGhost,origIdx}=item;
        if(isGhost){
          return(<div key="ghost" style={{width:52,height:52,borderRadius:10,position:"relative",
            border:`2px dashed ${T.accent}60`,background:`rgba(232,115,74,0.08)`,
            display:"flex",alignItems:"center",justifyContent:"center",
            fontFamily:"'Outfit',sans-serif",fontSize:d.letter==="Qu"?16:20,fontWeight:800,
            color:T.dieText,opacity:0.35,flexShrink:0,
          }}>
            {d.letter}
            <span style={{position:"absolute",bottom:1,right:2,fontSize:12,opacity:0.7,fontWeight:800}}>{getLetterValue(d.letter)}</span>
          </div>);
        }
        return(<div key={d.id} data-keepidx={origIdx} ref={el=>slotRefs.current[origIdx]=el}
          style={{display:"flex",alignItems:"center"}}>
          <Die die={d} isKept onClick={null} size={52}/>
        </div>);
      })}
      {/* Floating cursor ghost */}
      {dragIdx!==null&&pointerPos&&keepLine[dragIdx]&&(
        <div style={{position:"fixed",left:pointerPos.x-26,top:pointerPos.y-26,width:52,height:52,borderRadius:10,
          border:`2px solid ${T.accent}`,background:T.dieKeptBg,color:T.dieText,
          fontFamily:"'Outfit',sans-serif",fontSize:keepLine[dragIdx].letter==="Qu"?16:20,fontWeight:800,
          display:"flex",alignItems:"center",justifyContent:"center",
          boxShadow:`0 8px 24px rgba(0,0,0,0.4), 0 0 20px ${T.accentGlow}`,pointerEvents:"none",zIndex:100,transform:"scale(1.08) rotate(2deg)"}}>
          {keepLine[dragIdx].letter}
          <span style={{position:"absolute",bottom:1,right:2,fontSize:12,opacity:0.7,fontWeight:800}}>{getLetterValue(keepLine[dragIdx].letter)}</span>
        </div>
      )}
    </div>
  );
}

// ═══════════════════════════════════════════
// ROLL ARENA (3D dice, scatter, cup)
// ═══════════════════════════════════════════
function RollArena({rollPool,keepLine,onKeepDie,onKeepDieAt,keepLineRef,setIncomingDrag,rollPhase,waitMessage,waitSub}){
  const arenaRef=useRef(null);
  const[positions,setPositions]=useState({});
  const[scattered,setScattered]=useState(true);
  const[dragDie,setDragDie]=useState(null);
  const[pointerPos,setPointerPos]=useState(null);
  const dragStartR=useRef(null);const draggingR=useRef(false);

  useEffect(()=>{
    if(rollPhase==="scattering"){
      const el=arenaRef.current;if(!el)return;
      const w=el.offsetWidth,h=el.offsetHeight;
      const ds=54,margin=12,minDist=ds+18;
      const newPos={};
      rollPool.forEach((d,i)=>{
        let x,y,tries=0;
        const minX=w*0.2;
        do{
          x=minX+Math.random()*(w-ds-margin-minX);
          y=margin+Math.random()*(h-ds-margin*2);
          tries++;
        }while(tries<120&&Object.values(newPos).some(p=>{
          const dx=p.x-x,dy=p.y-y;
          return Math.sqrt(dx*dx+dy*dy)<minDist;
        }));
        newPos[d.id]={x,y,rotation:Math.random()*30-15,tiltX:Math.random()*14-7,tiltY:Math.random()*14-7,delay:i*0.06+Math.random()*0.12};
      });
      setPositions(newPos);setScattered(false);
      requestAnimationFrame(()=>requestAnimationFrame(()=>setScattered(true)));
    }
    if(rollPhase==="settled")setScattered(true);
  },[rollPhase]);

  // Generate positions for returned dice that don't have one yet
  const rollIds=rollPool.map(d=>d.id).join(",");
  useEffect(()=>{
    if(rollPhase!=="settled"&&rollPhase!=="waiting")return;
    const missing=rollPool.filter(d=>!positions[d.id]);
    if(missing.length===0)return;
    const el=arenaRef.current;if(!el)return;
    const w=el.offsetWidth,h=el.offsetHeight;
    const ds=54,margin=12,minDist=ds+18;
    // Gather all existing positions for dice still in rollPool
    const occupied=rollPool.filter(d=>positions[d.id]).map(d=>positions[d.id]);
    const newPos={...positions};
    missing.forEach(d=>{
      let x,y,tries=0;
      const minX=w*0.15;
      const allPlaced=[...occupied];
      do{
        x=minX+Math.random()*(w-ds-margin-minX);
        y=margin+Math.random()*(h-ds-margin*2);
        tries++;
      }while(tries<120&&allPlaced.some(p=>{
        const dx=p.x-x,dy=p.y-y;
        return Math.sqrt(dx*dx+dy*dy)<minDist;
      }));
      const entry={x,y,rotation:Math.random()*30-15,tiltX:Math.random()*14-7,tiltY:Math.random()*14-7,delay:0};
      newPos[d.id]=entry;
      occupied.push(entry);
    });
    setPositions(newPos);
  },[rollIds,rollPhase]);

  function computeInsertIdx(px){
    const keepEl=keepLineRef?.current;if(!keepEl)return keepLine.length;
    const slots=keepEl.querySelectorAll('[data-keepidx]');let ins=keepLine.length;
    for(let i=0;i<slots.length;i++){const r=slots[i].getBoundingClientRect();if(px<r.left+r.width/2){ins=parseInt(slots[i].dataset.keepidx);break;}}
    return ins;
  }
  function handlePointerDown(e,die){if(rollPhase!=="settled")return;dragStartR.current={x:e.clientX,y:e.clientY,die};draggingR.current=false;}
  function handlePointerMove(e){
    if(!dragStartR.current)return;
    const dx=e.clientX-dragStartR.current.x,dy=e.clientY-dragStartR.current.y;
    if(!draggingR.current&&Math.sqrt(dx*dx+dy*dy)>8){draggingR.current=true;setDragDie(dragStartR.current.die);e.target.setPointerCapture?.(e.pointerId);}
    if(draggingR.current){setPointerPos({x:e.clientX,y:e.clientY});setIncomingDrag({die:dragStartR.current.die,insertIdx:computeInsertIdx(e.clientX)});}
  }
  function handlePointerUp(e){
    if(draggingR.current&&dragDie){onKeepDieAt(dragDie,computeInsertIdx(pointerPos?.x??0));}
    else if(!draggingR.current&&dragStartR.current){onKeepDie(dragStartR.current.die);}
    setDragDie(null);setPointerPos(null);dragStartR.current=null;draggingR.current=false;setIncomingDrag(null);
  }

  const arenaH=230;
  const aw=arenaRef.current?.offsetWidth||340;

  return(
    <div ref={arenaRef} onPointerMove={handlePointerMove}
      style={{position:"relative",height:arenaH,borderRadius:18,overflow:"hidden",touchAction:"none",userSelect:"none",
        background:"radial-gradient(ellipse at 40% 35%, rgba(35,40,78,0.95) 0%, rgba(18,20,48,0.98) 100%)",
        border:"2px solid rgba(255,255,255,0.06)",
        boxShadow:"inset 0 2px 24px rgba(0,0,0,0.4), inset 0 -1px 8px rgba(255,255,255,0.02), 0 4px 16px rgba(0,0,0,0.3)"}}>
      {/* Felt texture */}
      <div style={{position:"absolute",inset:0,
        background:"radial-gradient(circle at 30% 30%, rgba(255,255,255,0.025) 0%, transparent 50%), radial-gradient(circle at 75% 70%, rgba(232,115,74,0.015) 0%, transparent 40%)",
        pointerEvents:"none"}}/>
      {/* Inner edge lip */}
      <div style={{position:"absolute",inset:0,borderRadius:16,
        boxShadow:"inset 0 0 0 3px rgba(60,65,110,0.5), inset 0 0 0 4px rgba(255,255,255,0.03)",
        pointerEvents:"none"}}/>

      {/* Cup animation */}
      {rollPhase==="cupping"&&(
        <div style={{position:"absolute",left:0,top:"50%",zIndex:10,pointerEvents:"none",
          animation:"cupEnter 1.1s ease-in-out forwards"}}>
          <div style={{width:94,height:78,position:"relative",
            background:"linear-gradient(165deg, #937538, #6B5522, #7B6328, #937538)",
            borderRadius:"6px 6px 18px 18px",border:"2px solid #A08940",
            boxShadow:"0 6px 24px rgba(0,0,0,0.5), inset 0 2px 10px rgba(255,255,255,0.12), inset 0 -4px 8px rgba(0,0,0,0.2)",
            transformOrigin:"right bottom"}}>
            <div style={{position:"absolute",top:-6,left:-7,right:-7,height:12,
              background:"linear-gradient(180deg, #B09548, #8B7332)",
              borderRadius:"6px 6px 3px 3px",border:"2px solid #C0A955",
              boxShadow:"0 2px 4px rgba(0,0,0,0.3)"}}/>
            <div style={{position:"absolute",top:20,left:"16%",right:"16%",height:1,background:"rgba(0,0,0,0.1)"}}/>
            <div style={{position:"absolute",top:40,left:"12%",right:"12%",height:1,background:"rgba(0,0,0,0.07)"}}/>
            <div style={{position:"absolute",top:58,left:"18%",right:"18%",height:1,background:"rgba(0,0,0,0.05)"}}/>
            <div style={{position:"absolute",top:6,left:8,right:8,height:20,borderRadius:"50%",
              background:"radial-gradient(ellipse, rgba(0,0,0,0.35), transparent)"}}/>
          </div>
        </div>
      )}

      {/* Empty state */}
      {rollPool.length===0&&rollPhase==="settled"&&!waitMessage&&(
        <div style={{position:"absolute",inset:0,display:"flex",alignItems:"center",justifyContent:"center",
          color:T.textFaint,fontSize:12,opacity:0.6}}>All dice kept!</div>
      )}

      {/* Waiting to roll message */}
      {waitMessage&&rollPhase==="waiting"&&(
        <div style={{position:"absolute",inset:0,display:"flex",flexDirection:"column",alignItems:"center",justifyContent:"center",
          zIndex:5,pointerEvents:"none"}}>
          <div style={{fontFamily:"'Righteous',cursive",fontSize:24,color:T.accent,textShadow:`0 0 20px ${T.accentGlow}`,
            marginBottom:4,animation:"slideUp 0.4s ease-out"}}>{waitMessage}</div>
          {waitSub&&<div style={{fontSize:11,color:T.textDim,marginBottom:8,animation:"slideUp 0.45s ease-out",letterSpacing:1}}>{waitSub}</div>}
          <div style={{fontSize:13,color:T.text,animation:"slideUp 0.5s ease-out",letterSpacing:1,
            fontWeight:600}}>Please roll 🎲</div>
        </div>
      )}

      {/* Scattered 3D dice */}
      {rollPool.map((d,i)=>{
        const pos=positions[d.id]||{x:aw/2-27,y:arenaH/2-27,rotation:0,tiltX:0,tiltY:0,delay:0};
        const isHidden=rollPhase==="cupping";
        const atCenter=rollPhase==="scattering"&&!scattered;
        // Dice originate from where cup tips (roughly 35% from left, vertically centered)
        const originX=aw*0.35-27,originY=arenaH/2-27;
        return(
          <div key={d.id} onPointerDown={e=>handlePointerDown(e,d)} onPointerUp={handlePointerUp}
            style={{position:"absolute",left:pos.x,top:pos.y,
              transform:atCenter?`translate(${originX-pos.x}px,${originY-pos.y}px) scale(0.2) rotate(540deg)`:`rotate(${pos.rotation}deg) scale(1)`,
              opacity:isHidden?0:(dragDie?.id===d.id?0.3:1),
              transition:(rollPhase==="scattering"&&scattered)
                ?`transform 0.75s cubic-bezier(0.12,0.8,0.32,1.08) ${pos.delay}s, opacity 0.25s ${pos.delay}s`
                :rollPhase==="settled"?"transform 0.5s ease-out, opacity 0.2s, left 0.5s ease-out, top 0.5s ease-out":"opacity 0.2s",
              pointerEvents:rollPhase==="settled"?"auto":"none",
              cursor:"pointer",zIndex:dragDie?.id===d.id?50:1,
              filter:rollPhase==="settled"?"drop-shadow(3px 4px 6px rgba(0,0,0,0.4))":"none",
            }}>
            <Die3D die={d} size={54} spinning={rollPhase==="scattering"&&scattered} tiltX={pos.tiltX} tiltY={pos.tiltY}/>
          </div>
        );
      })}

      {/* Floating drag ghost */}
      {dragDie&&pointerPos&&(
        <div style={{position:"fixed",left:pointerPos.x-27,top:pointerPos.y-27,width:54,height:54,borderRadius:10,
          border:"2px solid rgba(232,115,74,0.5)",background:"linear-gradient(145deg, #f5f0e8, #e8e0d0)",color:"#1a1d3a",
          fontFamily:"'Outfit',sans-serif",fontSize:dragDie.letter==="Qu"?16:20,fontWeight:800,
          display:"flex",alignItems:"center",justifyContent:"center",
          boxShadow:"0 8px 24px rgba(0,0,0,0.5), 0 0 16px rgba(232,115,74,0.2)",pointerEvents:"none",zIndex:100,transform:"scale(1.12) rotate(-3deg)"}}>
          {dragDie.letter}
          <span style={{position:"absolute",bottom:1,right:2,fontSize:12,opacity:0.7,fontWeight:800}}>{getLetterValue(dragDie.letter)}</span>
        </div>
      )}
    </div>
  );
}


// ═══════════════════════════════════════════
// ANIMATION STYLES
// ═══════════════════════════════════════════
const ANIM_CSS = `
@keyframes cubeRoll {
  0% { transform: rotateX(720deg) rotateY(540deg) rotateZ(180deg); }
  55% { transform: rotateX(-18deg) rotateY(12deg) rotateZ(-5deg); }
  75% { transform: rotateX(6deg) rotateY(-4deg) rotateZ(2deg); }
  100% { transform: rotateX(0deg) rotateY(0deg) rotateZ(0deg); }
}
@keyframes cupEnter {
  0% { left: -100px; top: 50%; transform: translateY(-50%) rotate(-15deg) scale(0.7); opacity:0; }
  15% { left: 22%; top: 50%; transform: translateY(-50%) rotate(2deg) scale(1); opacity:1; }
  22% { left: 22%; transform: translateY(-50%) rotate(-5deg); }
  29% { left: 22%; transform: translateY(-50%) rotate(6deg); }
  36% { left: 22%; transform: translateY(-50%) rotate(-4deg); }
  43% { left: 22%; transform: translateY(-50%) rotate(3deg); }
  50% { left: 22%; transform: translateY(-50%) rotate(-2deg); }
  58% { left: 22%; transform: translateY(-50%) rotate(0deg); opacity:1; }
  78% { left: 26%; transform: translateY(-50%) rotate(105deg) translateX(20px); opacity:0.85; }
  100% { left: 28%; transform: translateY(-50%) rotate(140deg) translateX(40px) translateY(-10px); opacity:0; }
}
@keyframes dieShadow {
  0%,100% { box-shadow: 3px 3px 8px rgba(0,0,0,0.35); }
  50% { box-shadow: 2px 5px 12px rgba(0,0,0,0.45); }
}
@keyframes scorePop {
  0% { transform: translateY(0) scale(0.5); opacity: 0; }
  12% { transform: translateY(-15px) scale(1.4); opacity: 1; }
  30% { transform: translateY(-40px) scale(1.1); opacity: 1; }
  70% { transform: translateY(-55vh) scale(1); opacity: 0.9; }
  100% { transform: translateY(-70vh) scale(0.7); opacity: 0; }
}
@keyframes eligiblePulse {
  0%, 100% { box-shadow: 0 0 0 0 rgba(86,216,156,0); }
  50% { box-shadow: 0 0 12px 2px rgba(86,216,156,0.25); }
}
@keyframes confettiFall {
  0% { transform: translateY(-10px) rotate(0deg); opacity: 1; }
  100% { transform: translateY(100vh) rotate(720deg); opacity: 0; }
}
@keyframes titleGlow {
  0%, 100% { filter: brightness(1); text-shadow: 0 0 20px rgba(232,115,74,0.3); }
  50% { filter: brightness(1.3); text-shadow: 0 0 40px rgba(232,115,74,0.6), 0 0 80px rgba(240,192,64,0.3); }
}
@keyframes slideUp {
  0% { transform: translateY(20px); opacity: 0; }
  100% { transform: translateY(0); opacity: 1; }
}
@keyframes bounceIn {
  0% { transform: scale(0); opacity: 0; }
  60% { transform: scale(1.2); opacity: 1; }
  100% { transform: scale(1); }
}
@media (min-width: 900px) {
  .wz-game-layout {
    display: flex !important;
    flex-direction: row !important;
    gap: 24px;
    align-items: flex-start;
  }
  .wz-roll-arena {
    flex: 1;
    min-width: 0;
  }
  .wz-scorecard-panel {
    width: 340px;
    flex-shrink: 0;
    position: sticky;
    top: 12px;
    max-height: calc(100vh - 24px);
    overflow-y: auto;
  }
  .wz-scorecard-toggle { display: none !important; }
}
`;

function ConfettiOverlay(){
  const particles=useRef(Array.from({length:50},(_,i)=>({
    id:i,left:Math.random()*100,delay:Math.random()*2,duration:1.5+Math.random()*2,
    color:["#e8734a","#f0c040","#56d89c","#6c8dff","#ff6b9d","#a78bfa"][Math.floor(Math.random()*6)],
    size:4+Math.random()*8,shape:Math.random()>0.5?"circle":"rect",
  })));
  return(
    <div style={{position:"fixed",inset:0,pointerEvents:"none",zIndex:2000,overflow:"hidden"}}>
      {particles.current.map(p=>(
        <div key={p.id} style={{
          position:"absolute",top:-10,left:`${p.left}%`,
          width:p.size,height:p.shape==="rect"?p.size*1.5:p.size,
          borderRadius:p.shape==="circle"?"50%":"2px",
          background:p.color,
          animation:`confettiFall ${p.duration}s ${p.delay}s ease-in forwards`,
          opacity:0,
        }}/>
      ))}
    </div>
  );
}

function ScorePopAnimation({score,onDone}){
  useEffect(()=>{const t=setTimeout(onDone,1600);return()=>clearTimeout(t);},[onDone]);
  return(
    <div style={{position:"fixed",bottom:"25%",left:"50%",transform:"translateX(-50%)",
      zIndex:1500,pointerEvents:"none",
      animation:"scorePop 1.6s ease-out forwards",
      fontFamily:"'Righteous',cursive",fontSize:52,fontWeight:900,
      color:"#56d89c",textShadow:"0 0 20px rgba(86,216,156,0.5), 0 2px 10px rgba(0,0,0,0.3)",
    }}>+{score}</div>
  );
}

// ═══════════════════════════════════════════
// MAIN COMPONENT
// ═══════════════════════════════════════════
export default function Wordtzee(){
  // Game setup
  const[phase,setPhase]=useState("menu"); // menu, setup, playing, computerTurn, gameover
  const[difficulty,setDifficulty]=useState("easy");
  const[numPlayers,setNumPlayers]=useState(1);
  const[hasComputer,setHasComputer]=useState(false);
  const[playerNames,setPlayerNames]=useState(["Player 1"]);

  // Game state
  const[categories,setCategories]=useState([]);
  const[rollPool,setRollPool]=useState([]);
  const[keepLine,setKeepLine]=useState([]);
  const[rollsLeft,setRollsLeft]=useState(MAX_REROLLS);
  const[round,setRound]=useState(1);
  const[currentPlayer,setCurrentPlayer]=useState(0);
  const[allScores,setAllScores]=useState({}); // { playerIdx: { catId: {...} } }
  const[word,setWord]=useState("");
  const[word2,setWord2]=useState("");
  const[twoForOneMode,setTwoForOneMode]=useState(false);
  const[validationResult,setValidationResult]=useState(null);
  const[validating,setValidating]=useState(false);
  const[eligible,setEligible]=useState({});
  const[message,setMessage]=useState("");
  const[hasRolled,setHasRolled]=useState(false);
  const[showScorecard,setShowScorecard]=useState(true);
  const[confirmModal,setConfirmModal]=useState(null);
  const[showMenu,setShowMenu]=useState(false);
  const[showInstructions,setShowInstructions]=useState(false);
  const[incomingDrag,setIncomingDrag]=useState(null);
  const[rollPhase,setRollPhase]=useState("settled"); // cupping, scattering, settled
  const[scorePop,setScorePop]=useState(null);
  const[confetti,setConfetti]=useState(false);
  const[lastScoredCat,setLastScoredCat]=useState(null);
  const[scorecardFilter,setScorecardFilter]=useState("all"); // all, upper, lower, needed, scored
  const[customNames,setCustomNames]=useState(["","","",""]); // user-entered names
  const keepLineRef=useRef(null);
  const topRef=useRef(null);
  const rollTimers=useRef([]);

  const scores=allScores[currentPlayer]||{};
  const allDiceLetters=[...rollPool,...keepLine].map(d=>d.letter);
  const totalPlayers=numPlayers+(hasComputer?1:0);
  const keepLineWord=keepLine.map(d=>d.letter==="Qu"?"qu":d.letter.toLowerCase()).join("");

  // Auto-sync word from keep line (when not in two-for-one mode)
  useEffect(()=>{
    if(!twoForOneMode){
      setWord(keepLineWord);
      setValidationResult(null);setEligible({});setMessage("");
    }
  },[keepLineWord,twoForOneMode]);

  useEffect(()=>{
    if(!localStorage.getItem("wordtzee-seen-instructions")){
      setShowInstructions(true);
      localStorage.setItem("wordtzee-seen-instructions","1");
    }
  },[]);

  // Force scorecard visible on desktop-width viewports
  useEffect(()=>{
    const mq=window.matchMedia("(min-width: 900px)");
    const handler=(e)=>{if(e.matches)setShowScorecard(true);};
    mq.addEventListener("change",handler);
    if(mq.matches)setShowScorecard(true);
    return()=>mq.removeEventListener("change",handler);
  },[]);

  function getPlayerScore(pIdx){
    const s=allScores[pIdx]||{};
    return Object.values(s).reduce((sum,v)=>sum+(v?.score||0),0);
  }
  function getPlayerUpperScore(pIdx){
    const s=allScores[pIdx]||{};
    return Object.entries(s).filter(([k])=>categories.find(c=>c.id===k)?.section==="upper").reduce((sum,[,v])=>sum+(v?.score||0),0);
  }

  const totalScore=getPlayerScore(currentPlayer);
  const upperScore=getPlayerUpperScore(currentPlayer);
  const upperBonus=upperScore>=50?25:0;

  function startGame(){
    const cats=getCategoriesForDifficulty(difficulty);
    setCategories(cats);
    const names=[];
    for(let i=0;i<numPlayers;i++){
      const custom=customNames[i]?.trim();
      names.push(custom||( numPlayers===1?"Player":`Player ${i+1}`));
    }
    if(hasComputer)names.push("Computer");
    setPlayerNames(names);
    const sc={};names.forEach((_,i)=>sc[i]={});setAllScores(sc);
    setRound(1);setCurrentPlayer(0);setPhase("playing");setupTurn();
  }

  function clearRollTimers(){rollTimers.current.forEach(clearTimeout);rollTimers.current=[];}

  function setupTurn(){
    clearRollTimers();
    setRollPhase("waiting");
    setRollPool([]);setKeepLine([]);setRollsLeft(MAX_REROLLS);
    setWord("");setWord2("");setTwoForOneMode(false);setValidationResult(null);
    setEligible({});setMessage("");setHasRolled(false);
  }

  function doRoll(){
    clearRollTimers();
    setRollPhase("cupping");
    setRollPool(rollAllDice());setKeepLine([]);
    setHasRolled(true);
    rollTimers.current.push(setTimeout(()=>setRollPhase("scattering"),1050));
    rollTimers.current.push(setTimeout(()=>setRollPhase("settled"),2000));
  }

  function handleKeepDie(die){setRollPool(p=>p.filter(d=>d.id!==die.id));setKeepLine(k=>[...k,die]);clearValidation();}
  function handleKeepDieAt(die,idx){setRollPool(p=>p.filter(d=>d.id!==die.id));setKeepLine(k=>{const n=[...k];n.splice(idx,0,die);return n;});clearValidation();}
  function handleUnkeepDie(die){setKeepLine(k=>k.filter(d=>d.id!==die.id));setRollPool(p=>[...p,die]);clearValidation();}
  function clearValidation(){setValidationResult(null);setEligible({});setMessage("");}

  function handleReroll(){
    if(rollsLeft<=0||rollPhase!=="settled")return;
    clearRollTimers();
    setRollPhase("cupping");
    setRollPool(p=>shuffleArray(p.map(d=>rerollSingleDie(d))));
    setRollsLeft(r=>r-1);clearValidation();
    rollTimers.current.push(setTimeout(()=>setRollPhase("scattering"),1050));
    rollTimers.current.push(setTimeout(()=>setRollPhase("settled"),2000));
  }

  async function handleValidate(){
    setValidating(true);setMessage("");
    if(twoForOneMode){
      if(!word.trim()||!word2.trim()){setMessage("Enter both words!");setValidating(false);return;}
      const wl1=wordToLetters(word.trim()),wl2=wordToLetters(word2.trim()),combined=[...wl1,...wl2];
      if(combined.length!==7||!canFormWord(combined,allDiceLetters)){setMessage("Both words must use all 7 dice!");setValidating(false);return;}
      const[r1,r2]=await Promise.all([validateWord(word.trim()),validateWord(word2.trim())]);
      if(!r1.valid||!r2.valid){setMessage(`${!r1.valid?`"${word}" `:""}${!r2.valid?`"${word2}" `:""}not in dictionary!`);setValidating(false);return;}
      const lv=sumLetterValues(combined);const tfoCat=categories.find(c=>c.type==="twoforone");
      if(tfoCat&&!scores[tfoCat.id]){setEligible({[tfoCat.id]:15+lv});setValidationResult({word:`${word.trim()} + ${word2.trim()}`,letters:combined,pos:[]});}
      else setMessage("Two-for-One already filled!");
      setValidating(false);return;
    }
    // Normal mode: word is built from keep line
    if(keepLine.length===0){setMessage("Move dice to the keep line first!");setValidating(false);return;}
    const theWord=keepLineWord;
    const wl=keepLine.map(d=>d.letter);
    const result=await validateWord(theWord);
    if(!result.valid){setMessage(`"${theWord.toUpperCase()}" is not in the dictionary!`);setValidating(false);return;}
    setValidationResult({word:theWord,letters:wl,pos:result.pos});
    const elig={};
    for(const cat of categories){if(scores[cat.id]!==undefined||cat.type==="twoforone")continue;if(checkCategoryEligibility(theWord,wl,cat,result.pos,allDiceLetters))elig[cat.id]=calcScore(theWord,wl,cat);}
    setEligible(elig);
    if(Object.keys(elig).length===0)setMessage("Valid word, but no open categories match. Try another or scratch.");
    setValidating(false);
  }

  function attemptAssign(catId){
    const score=eligible[catId];if(score===undefined)return;
    if(score===0){setConfirmModal({message:`"${validationResult.word}" earns 0 points for ${categories.find(c=>c.id===catId)?.name}. Sure?`,onConfirm:()=>{doAssign(catId,score);setConfirmModal(null);},onCancel:()=>setConfirmModal(null)});}
    else doAssign(catId,score);
  }

  function doAssign(catId,score){
    const cat=categories.find(c=>c.id===catId);
    setAllScores(a=>({...a,[currentPlayer]:{...a[currentPlayer],[catId]:{word:validationResult.word,score,letters:validationResult.letters,breakdown:getScoreBreakdown(validationResult.word,validationResult.letters,cat)}}}));
    // Trigger score pop animation
    if(score>0){setScorePop(score);setTimeout(()=>setScorePop(null),1800);}
    // Trigger confetti for Wordtzee category
    if(cat.type==="wordtzee"){setConfetti(true);setTimeout(()=>setConfetti(false),3500);}
    setLastScoredCat(catId);setTimeout(()=>setLastScoredCat(null),600);
    // Scroll back to top with the floating score
    setTimeout(()=>{
      if(topRef.current)topRef.current.scrollIntoView({behavior:"smooth",block:"start"});
    },300);
    setTimeout(()=>advanceTurn(),score>0?1200:400);
  }

  function attemptScratch(catId){
    if(scores[catId]!==undefined)return;const cat=categories.find(c=>c.id===catId);
    setConfirmModal({message:`Scratch "${cat?.name}"? Scores 0, can't undo.`,
      onConfirm:()=>{setAllScores(a=>({...a,[currentPlayer]:{...a[currentPlayer],[catId]:{word:"—",score:0,letters:[],breakdown:[]}}}));setTimeout(()=>{if(topRef.current)topRef.current.scrollIntoView({behavior:"smooth",block:"start"});},200);advanceTurn();setConfirmModal(null);},onCancel:()=>setConfirmModal(null)});
  }

  function advanceTurn(){
    const nextPlayer=(currentPlayer+1)%totalPlayers;
    const roundDone=nextPlayer===0;
    if(roundDone&&round>=TOTAL_ROUNDS){setPhase("gameover");return;}
    if(roundDone)setRound(r=>r+1);
    setCurrentPlayer(nextPlayer);
    // Check if next player is computer — auto-roll for them
    if(hasComputer&&nextPlayer===totalPlayers-1){
      setupTurn();
      setTimeout(()=>runComputerTurn(nextPlayer,roundDone?round+1:round),600);
    } else {
      setupTurn();
    }
  }

  function runComputerTurn(pIdx,rnd){
    setPhase("computerTurn");
    const dice=rollAllDice();
    const letters=dice.map(d=>d.letter);
    const pScores=allScores[pIdx]||{};
    const result=computerFindWord(letters,categories,pScores);
    setTimeout(()=>{
      if(result){
        const cat=categories.find(c=>c.id===result.catId);
        setAllScores(a=>({...a,[pIdx]:{...a[pIdx],[result.catId]:{word:result.word,score:result.score,letters:result.letters,breakdown:getScoreBreakdown(result.word,result.letters,cat)}}}));
        setMessage(`Computer played "${result.word}" → ${cat.name} for ${result.score} pts`);
      }else{
        // Scratch cheapest open category
        const open=categories.filter(c=>!pScores[c.id]);
        if(open.length>0){
          setAllScores(a=>({...a,[pIdx]:{...a[pIdx],[open[0].id]:{word:"—",score:0,letters:[],breakdown:[]}}}));
          setMessage(`Computer scratched ${open[0].name}`);
        }
      }
      // Advance to next player
      const nextP=(pIdx+1)%totalPlayers;
      const roundDone2=nextP===0;
      if(roundDone2&&rnd>=TOTAL_ROUNDS){setTimeout(()=>setPhase("gameover"),1200);return;}
      if(roundDone2)setRound(r=>r+1);
      setTimeout(()=>{setCurrentPlayer(nextP);setPhase("playing");setupTurn();},1200);
    },1000);
  }

  function quitToMenu(){setPhase("menu");setConfirmModal(null);setShowMenu(false);}

  const diffColors={easy:T.easy,medium:T.medium,hard:T.hard};
  const btnStyle=(active,color)=>({padding:"12px 24px",borderRadius:12,border:active?`2px solid ${color}`:"2px solid rgba(255,255,255,0.06)",background:active?`${color}18`:"transparent",color:active?color:T.textDim,fontFamily:"'Outfit',sans-serif",fontSize:14,fontWeight:700,cursor:"pointer",textTransform:"uppercase",letterSpacing:2,transition:"all 0.2s"});

  // ── MENU SCREEN ──
  if(phase==="menu"){
    return(
      <div style={{fontFamily:"'Outfit',sans-serif",minHeight:"100vh",display:"flex",flexDirection:"column",alignItems:"center",justifyContent:"center",background:T.bg,color:T.text,padding:20,textAlign:"center"}}>
        <style>{ANIM_CSS}</style>
        <div style={{fontSize:11,letterSpacing:6,textTransform:"uppercase",color:T.accent,marginBottom:8,fontWeight:600,animation:"slideUp 0.5s ease-out"}}>Roll · Keep · Spell</div>
        <h1 style={{fontFamily:"'Righteous',cursive",fontSize:68,letterSpacing:5,background:"linear-gradient(135deg, #e8734a, #f0c040)",WebkitBackgroundClip:"text",WebkitTextFillColor:"transparent",margin:"0 0 6px",lineHeight:1,animation:"titleGlow 3s ease-in-out infinite"}}>WORDTZEE</h1>
        <p style={{fontSize:14,color:T.textDim,marginBottom:40,maxWidth:300,animation:"slideUp 0.7s ease-out"}}>A word-building dice game</p>
        <button onClick={()=>setPhase("setup")} style={{padding:"16px 52px",borderRadius:16,border:"none",background:T.accentGrad,color:"#fff",fontFamily:"'Righteous',cursive",fontSize:24,letterSpacing:3,cursor:"pointer",boxShadow:`0 4px 24px ${T.accentGlow}`,marginBottom:16,animation:"bounceIn 0.6s 0.3s ease-out both",transition:"transform 0.15s"}}
          onMouseEnter={e=>e.target.style.transform="scale(1.05)"} onMouseLeave={e=>e.target.style.transform="scale(1)"}>NEW GAME</button>
        <button onClick={()=>setShowInstructions(true)} style={{padding:"10px 36px",borderRadius:12,border:`1px solid ${T.textFaint}`,background:"transparent",color:T.textDim,fontFamily:"'Outfit',sans-serif",fontSize:13,fontWeight:600,cursor:"pointer",marginBottom:16,animation:"slideUp 0.8s ease-out",transition:"all 0.15s",letterSpacing:1}}
          onMouseEnter={e=>{e.target.style.color=T.accent;e.target.style.borderColor=T.accent;}} onMouseLeave={e=>{e.target.style.color=T.textDim;e.target.style.borderColor=T.textFaint;}}>How to Play</button>
        <div style={{fontSize:11,color:T.textDim,maxWidth:280,lineHeight:1.6,marginTop:8,animation:"slideUp 0.9s ease-out"}}>Roll 7 letter dice, keep and reorder to form words, fill 13 scoring categories.</div>
        {showInstructions&&<InstructionsModal onClose={()=>setShowInstructions(false)}/>}
      </div>
    );
  }

  // ── SETUP SCREEN ──
  if(phase==="setup"){
    return(
      <div style={{fontFamily:"'Outfit',sans-serif",minHeight:"100vh",display:"flex",flexDirection:"column",alignItems:"center",justifyContent:"center",background:T.bg,color:T.text,padding:20,textAlign:"center"}}>
        <style>{ANIM_CSS}</style>
        <h2 style={{fontFamily:"'Righteous',cursive",fontSize:32,background:"linear-gradient(135deg, #e8734a, #f0c040)",WebkitBackgroundClip:"text",WebkitTextFillColor:"transparent",marginBottom:32,animation:"slideUp 0.4s ease-out"}}>GAME SETUP</h2>

        {/* Difficulty */}
        <div style={{marginBottom:24}}>
          <div style={{fontSize:10,letterSpacing:3,textTransform:"uppercase",color:T.textDim,marginBottom:10,fontWeight:600}}>Difficulty</div>
          <div style={{display:"flex",gap:8}}>
            {["easy","medium","hard"].map(d=>(<button key={d} onClick={()=>setDifficulty(d)} style={btnStyle(difficulty===d,diffColors[d])}>{d}</button>))}
          </div>
          <p style={{fontSize:11,color:T.textDim,marginTop:8,maxWidth:300,lineHeight:1.5}}>
            {difficulty==="easy"?"Noun, Verb, Adjective + Free Word safety net.":difficulty==="medium"?"Verb & Adjective → Two-for-One & All Unique.":"Free Word → Compound Word. No safety net!"}
          </p>
        </div>

        {/* Players */}
        <div style={{marginBottom:24}}>
          <div style={{fontSize:10,letterSpacing:3,textTransform:"uppercase",color:T.textDim,marginBottom:10,fontWeight:600}}>Players</div>
          <div style={{display:"flex",gap:8,justifyContent:"center",marginBottom:12}}>
            {[1,2,3,4].map(n=>(<button key={n} onClick={()=>setNumPlayers(n)} style={btnStyle(numPlayers===n,T.accent)}>{n}</button>))}
          </div>
          {/* Player name inputs */}
          <div style={{display:"flex",flexDirection:"column",gap:8,alignItems:"center"}}>
            {Array.from({length:numPlayers},(_,i)=>(
              <input key={i} type="text" placeholder={numPlayers===1?"Your name (optional)":`Player ${i+1} name`}
                value={customNames[i]||""} onChange={e=>{const n=[...customNames];n[i]=e.target.value;setCustomNames(n);}}
                style={{width:220,padding:"8px 12px",borderRadius:10,border:"1px solid rgba(255,255,255,0.1)",background:"rgba(0,0,0,0.3)",
                  color:T.text,fontFamily:"'Outfit',sans-serif",fontSize:13,fontWeight:500,textAlign:"center",outline:"none"}}
                onFocus={e=>e.target.style.borderColor=T.accent} onBlur={e=>e.target.style.borderColor="rgba(255,255,255,0.1)"}
              />
            ))}
          </div>
        </div>

        {/* VS Computer */}
        <div style={{marginBottom:32}}>
          <button onClick={()=>setHasComputer(!hasComputer)}
            style={{padding:"10px 24px",borderRadius:12,border:hasComputer?`2px solid ${T.gold}`:"2px solid rgba(255,255,255,0.06)",
              background:hasComputer?`${T.gold}18`:"transparent",color:hasComputer?T.gold:T.textDim,
              fontFamily:"'Outfit',sans-serif",fontSize:13,fontWeight:700,cursor:"pointer",letterSpacing:1}}>
            {hasComputer?"✓ VS Computer":"+ Add Computer Opponent"}
          </button>
        </div>

        <div style={{display:"flex",gap:12}}>
          <button onClick={()=>setPhase("menu")} style={{padding:"14px 28px",borderRadius:14,border:`1px solid ${T.textFaint}`,background:"transparent",color:T.textDim,fontFamily:"'Outfit',sans-serif",fontSize:15,fontWeight:600,cursor:"pointer"}}>Back</button>
          <button onClick={startGame} style={{padding:"14px 44px",borderRadius:14,border:"none",background:T.accentGrad,color:"#fff",fontFamily:"'Righteous',cursive",fontSize:20,letterSpacing:2,cursor:"pointer",boxShadow:`0 4px 20px ${T.accentGlow}`}}>START</button>
        </div>
      </div>
    );
  }

  // ── GAME OVER ──
  if(phase==="gameover"){
    const rankings=Array.from({length:totalPlayers},(_,i)=>({idx:i,name:playerNames[i],score:getPlayerScore(i)+(getPlayerUpperScore(i)>=50?25:0)})).sort((a,b)=>b.score-a.score);
    return(
      <div style={{fontFamily:"'Outfit',sans-serif",minHeight:"100vh",display:"flex",flexDirection:"column",alignItems:"center",background:T.bg,color:T.text,padding:20,textAlign:"center"}}>
        <style>{ANIM_CSS}</style>
        <ConfettiOverlay/>
        <h1 style={{fontFamily:"'Righteous',cursive",fontSize:38,background:"linear-gradient(135deg, #e8734a, #f0c040)",WebkitBackgroundClip:"text",WebkitTextFillColor:"transparent",margin:"28px 0 16px",animation:"titleGlow 2s ease-in-out infinite"}}>GAME OVER</h1>
        {/* Rankings */}
        {rankings.map((r,i)=>(
          <div key={r.idx} style={{display:"flex",alignItems:"center",gap:12,marginBottom:8,padding:"10px 20px",borderRadius:12,
            background:i===0?"rgba(232,115,74,0.1)":"rgba(255,255,255,0.02)",
            border:i===0?`1px solid ${T.accent}40`:"1px solid transparent",minWidth:200,
            animation:`slideUp 0.4s ${0.2+i*0.15}s ease-out both`}}>
            <span style={{fontSize:20,fontWeight:900,color:i===0?T.gold:T.textDim,width:28}}>{i===0?"🏆":`#${i+1}`}</span>
            <span style={{flex:1,textAlign:"left",fontWeight:600,fontSize:14}}>{r.name}</span>
            <span style={{fontSize:22,fontWeight:900,color:i===0?T.accent:T.text}}>{r.score}</span>
          </div>
        ))}
        {/* Detail for winner */}
        <div style={{width:"100%",maxWidth:420,marginTop:16,marginBottom:24}}>
          <div style={{fontSize:10,color:T.textDim,marginBottom:8,letterSpacing:2,textTransform:"uppercase"}}>{rankings[0]?.name}'s Scorecard</div>
          {categories.filter(c=>(allScores[rankings[0]?.idx]||{})[c.id]).map(c=>{
            const s=(allScores[rankings[0]?.idx]||{})[c.id];
            return(<ScoreTooltip key={c.id} breakdown={s?.breakdown} total={s?.score}>
              <div style={{display:"flex",justifyContent:"space-between",padding:"5px 14px",borderBottom:"1px solid rgba(255,255,255,0.04)",fontSize:12,cursor:"default",width:"100%"}}>
                <span style={{opacity:0.5}}>{c.name}</span>
                <span><span style={{opacity:0.3,marginRight:8,fontSize:10}}>{s?.word}</span><span style={{fontWeight:700,color:s?.score>0?T.success:T.danger}}>{s?.score}</span></span>
              </div>
            </ScoreTooltip>);
          })}
        </div>
        <div style={{display:"flex",gap:12}}>
          <button onClick={()=>setPhase("setup")} style={{padding:"12px 28px",borderRadius:12,border:`1px solid ${T.textFaint}`,background:"transparent",color:T.textDim,fontFamily:"'Outfit',sans-serif",fontSize:14,fontWeight:600,cursor:"pointer"}}>New Game</button>
          <button onClick={quitToMenu} style={{padding:"12px 28px",borderRadius:12,border:"none",background:T.accentGrad,color:"#fff",fontFamily:"'Outfit',sans-serif",fontSize:14,fontWeight:700,cursor:"pointer"}}>Main Menu</button>
        </div>
      </div>
    );
  }

  // ── COMPUTER TURN ──
  if(phase==="computerTurn"){
    return(
      <div style={{fontFamily:"'Outfit',sans-serif",minHeight:"100vh",display:"flex",flexDirection:"column",alignItems:"center",justifyContent:"center",background:T.bg,color:T.text,padding:20,textAlign:"center"}}>
        <div style={{fontSize:40,marginBottom:16}}>🤖</div>
        <h2 style={{fontFamily:"'Righteous',cursive",fontSize:28,color:T.gold,marginBottom:8}}>Computer's Turn</h2>
        <p style={{color:T.textDim,fontSize:14}}>Thinking...</p>
        {message&&<p style={{color:T.success,fontSize:14,marginTop:12,fontWeight:600}}>{message}</p>}
      </div>
    );
  }

  // ── PLAYING ──
  return(
    <div style={{fontFamily:"'Outfit',sans-serif",minHeight:"100vh",background:T.bg,color:T.text,padding:"12px 12px 140px"}}>
      <div ref={topRef}/>
      <style>{ANIM_CSS}</style>
      {confirmModal&&<ConfirmModal {...confirmModal}/>}
      {showInstructions&&<InstructionsModal onClose={()=>setShowInstructions(false)}/>}
      {confetti&&<ConfettiOverlay/>}
      {scorePop!==null&&<ScorePopAnimation score={scorePop} onDone={()=>setScorePop(null)}/>}

      {/* In-game menu overlay */}
      {showMenu&&(
        <div style={{position:"fixed",inset:0,background:"rgba(0,0,0,0.7)",display:"flex",alignItems:"center",justifyContent:"center",zIndex:1000,padding:20}}>
          <div style={{background:"linear-gradient(145deg, #1e2248, #252a52)",borderRadius:16,padding:24,maxWidth:320,width:"100%",border:"1px solid rgba(232,115,74,0.2)"}}>
            <h3 style={{fontFamily:"'Righteous',cursive",fontSize:22,color:T.accent,margin:"0 0 20px",textAlign:"center"}}>PAUSED</h3>
            <button onClick={()=>setShowMenu(false)} style={{width:"100%",padding:"12px",borderRadius:10,border:"none",background:T.accentGrad,color:"#fff",fontFamily:"'Outfit',sans-serif",fontSize:14,fontWeight:700,cursor:"pointer",marginBottom:8}}>Resume Game</button>
            <button onClick={()=>{setShowMenu(false);setShowInstructions(true);}} style={{width:"100%",padding:"12px",borderRadius:10,border:`1px solid ${T.textFaint}`,background:"transparent",color:T.text,fontFamily:"'Outfit',sans-serif",fontSize:14,fontWeight:600,cursor:"pointer",marginBottom:8}}>How to Play</button>
            <button onClick={()=>{setShowMenu(false);setPhase("setup");}} style={{width:"100%",padding:"12px",borderRadius:10,border:`1px solid ${T.textFaint}`,background:"transparent",color:T.text,fontFamily:"'Outfit',sans-serif",fontSize:14,fontWeight:600,cursor:"pointer",marginBottom:8}}>New Game</button>
            <button onClick={quitToMenu} style={{width:"100%",padding:"12px",borderRadius:10,border:`1px solid ${T.danger}40`,background:"transparent",color:T.danger,fontFamily:"'Outfit',sans-serif",fontSize:14,fontWeight:600,cursor:"pointer"}}>Quit to Menu</button>
          </div>
        </div>
      )}

      {/* Header */}
      <div style={{display:"flex",justifyContent:"space-between",alignItems:"center",marginBottom:6}}>
        <div style={{display:"flex",alignItems:"center",gap:8}}>
          <span onClick={()=>setShowMenu(true)} style={{fontFamily:"'Righteous',cursive",fontSize:20,background:"linear-gradient(135deg, #e8734a, #f0c040)",WebkitBackgroundClip:"text",WebkitTextFillColor:"transparent",letterSpacing:2,cursor:"pointer"}}>WORDTZEE</span>
          <span style={{fontSize:9,padding:"3px 7px",borderRadius:6,border:`1px solid ${diffColors[difficulty]}35`,color:diffColors[difficulty],fontWeight:700,textTransform:"uppercase",letterSpacing:1}}>{difficulty}</span>
        </div>
        <button onClick={()=>setShowMenu(true)} style={{background:"rgba(255,255,255,0.06)",border:"none",color:T.textDim,fontSize:18,cursor:"pointer",borderRadius:8,width:32,height:32,display:"flex",alignItems:"center",justifyContent:"center"}}>☰</button>
      </div>

      {/* Player bar */}
      {totalPlayers>1&&(
        <div style={{display:"flex",gap:4,marginBottom:8,overflowX:"auto"}}>
          {playerNames.map((name,i)=>{
            const ps=getPlayerScore(i)+(getPlayerUpperScore(i)>=50?25:0);
            return(<div key={i} style={{padding:"6px 12px",borderRadius:8,flex:1,textAlign:"center",
              background:currentPlayer===i?`${T.accent}20`:"rgba(255,255,255,0.02)",
              border:currentPlayer===i?`1px solid ${T.accent}40`:"1px solid transparent",
              transition:"all 0.2s"}}>
              <div style={{fontSize:11,fontWeight:currentPlayer===i?700:500,color:currentPlayer===i?T.accent:T.textDim}}>{name}</div>
              <div style={{fontSize:16,fontWeight:800,color:currentPlayer===i?T.text:T.textDim}}>{ps}</div>
            </div>);
          })}
        </div>
      )}

      {/* Single player score */}
      {totalPlayers===1&&(
        <div style={{display:"flex",justifyContent:"flex-end",marginBottom:4}}>
          <div style={{textAlign:"right"}}>
            <div style={{fontSize:22,fontWeight:900,background:"linear-gradient(135deg, #e8734a, #f0c040)",WebkitBackgroundClip:"text",WebkitTextFillColor:"transparent"}}>{totalScore+upperBonus}</div>
            <div style={{fontSize:8,color:T.textFaint,textTransform:"uppercase",letterSpacing:2}}>Score</div>
          </div>
        </div>
      )}

      {/* Game Layout — side-by-side on desktop */}
      <div className="wz-game-layout">
      <div className="wz-roll-arena">
      {/* Round & Rolls */}
      <div style={{display:"flex",justifyContent:"space-between",alignItems:"center",marginBottom:10}}>
        <div style={{fontSize:12,fontWeight:600}}>Round <span style={{color:T.accent}}>{round}</span><span style={{color:T.textFaint}}>/{TOTAL_ROUNDS}</span></div>
        <div style={{display:"flex",gap:5,alignItems:"center"}}>
          {[0,1,2].map(i=>(<div key={i} style={{width:7,height:7,borderRadius:"50%",background:i<rollsLeft?T.accent:"rgba(255,255,255,0.1)",boxShadow:i<rollsLeft?`0 0 6px ${T.accentGlow}`:"none",transition:"all 0.3s"}}/>))}
          <span style={{fontSize:9,color:T.textFaint,marginLeft:3}}>rolls</span>
        </div>
      </div>

      {/* ROLL ARENA */}
      <div style={{marginBottom:8}}>
        {rollPhase!=="waiting"&&<div style={{fontSize:9,textTransform:"uppercase",letterSpacing:2,color:T.textFaint,marginBottom:5,fontWeight:600}}>
          {rollPool.length>0?(rollPhase!=="settled"?"Rolling...":"Tap or drag to keep ↓"):"All dice kept!"}
        </div>}
        <RollArena rollPool={rollPool} keepLine={keepLine} onKeepDie={handleKeepDie} onKeepDieAt={handleKeepDieAt} keepLineRef={keepLineRef} setIncomingDrag={setIncomingDrag} rollPhase={rollPhase}
          waitMessage={totalPlayers>1?`${playerNames[currentPlayer]}'s Turn`:`Round ${round} of ${TOTAL_ROUNDS}`}
          waitSub={totalPlayers>1?`Round ${round} of ${TOTAL_ROUNDS}`:""}/>
      </div>

      {/* KEEP LINE */}
      {hasRolled&&<div style={{marginBottom:10}}>
        <div style={{fontSize:9,textTransform:"uppercase",letterSpacing:2,color:T.accent,marginBottom:5,fontWeight:600,display:"flex",justifyContent:"space-between"}}>
          <span>Your letters {keepLine.length>0&&`(${keepLine.length})`}</span>
          {keepLine.length>0&&(
            <span style={{color:T.textFaint,fontSize:9,cursor:"pointer",fontWeight:500}}
              onClick={()=>{setRollPool(p=>shuffleArray([...p,...keepLine]));setKeepLine([]);clearValidation();}}>
              ↩ Return all
            </span>
          )}
        </div>
        <div ref={keepLineRef}>
          <DraggableKeepLine keepLine={keepLine} setKeepLine={setKeepLine} onUnkeep={handleUnkeepDie} clearValidation={clearValidation} incomingDrag={incomingDrag}/>
        </div>
        {keepLine.length>1&&<div style={{fontSize:9,color:T.textFaint,textAlign:"center",marginTop:3}}>Drag to reorder · Tap to return</div>}
      </div>}

      {/* Roll / Reroll */}
      <div style={{textAlign:"center",marginBottom:12}}>
        {rollPhase==="waiting"?(
          <button onClick={doRoll} style={{
            padding:"12px 44px",borderRadius:12,border:"none",
            background:T.accentGrad,color:"#fff",
            fontFamily:"'Righteous',cursive",fontSize:18,fontWeight:700,cursor:"pointer",letterSpacing:2,
            boxShadow:`0 4px 20px ${T.accentGlow}`,
            animation:"bounceIn 0.5s ease-out 0.3s both"}}>
            🎲 ROLL
          </button>
        ):(
          <button onClick={handleReroll} disabled={rollsLeft<=0||rollPool.length===0||rollPhase!=="settled"} style={{
            padding:"9px 30px",borderRadius:12,border:"none",
            background:(rollsLeft>0&&rollPool.length>0&&rollPhase==="settled")?T.accentGrad:"rgba(255,255,255,0.05)",
            color:(rollsLeft>0&&rollPool.length>0&&rollPhase==="settled")?"#fff":T.textFaint,
            fontFamily:"'Outfit',sans-serif",fontSize:13,fontWeight:700,cursor:(rollsLeft>0&&rollPool.length>0&&rollPhase==="settled")?"pointer":"default",letterSpacing:1,
            boxShadow:(rollsLeft>0&&rollPool.length>0&&rollPhase==="settled")?`0 2px 12px ${T.accentGlow}`:"none"}}>
            {rollPhase!=="settled"?"ROLLING...":rollsLeft>0?(rollPool.length===0?"RETURN DICE TO REROLL":`REROLL${rollPool.length<7?` (${rollPool.length})`:""} — ${rollsLeft} left`):"NO REROLLS LEFT"}
          </button>
        )}
      </div>

      {/* Word Input */}
      {hasRolled&&<div style={{background:"rgba(255,255,255,0.025)",borderRadius:16,padding:14,marginBottom:10,border:"1px solid rgba(255,255,255,0.04)"}}>
        {!twoForOneMode?(
          <div style={{marginBottom:8}}>
            <div style={{padding:"9px 12px",borderRadius:10,border:`2px solid ${T.accent}18`,background:"rgba(0,0,0,0.25)",
              color:keepLineWord?T.text:T.textFaint,fontFamily:"'Outfit',sans-serif",fontSize:17,fontWeight:700,letterSpacing:3,
              textTransform:"uppercase",minHeight:22}}>
              {keepLineWord||"arrange dice above to spell a word"}
            </div>
          </div>
        ):(
          <div>
            <div style={{display:"flex",gap:8,marginBottom:8}}>
              <input type="text" value={word} onChange={e=>{setWord(e.target.value);clearValidation();}}
                placeholder="First word..."
                style={{flex:1,padding:"9px 12px",borderRadius:10,border:`2px solid ${T.accent}18`,background:"rgba(0,0,0,0.25)",color:T.text,fontFamily:"'Outfit',sans-serif",fontSize:17,fontWeight:700,letterSpacing:3,outline:"none",textTransform:"uppercase"}}
                onFocus={e=>e.target.style.borderColor=`${T.accent}60`} onBlur={e=>e.target.style.borderColor=`${T.accent}18`}/>
            </div>
            <div style={{marginBottom:8}}>
              <input type="text" value={word2} onChange={e=>{setWord2(e.target.value);clearValidation();}} placeholder="Second word..."
                style={{width:"100%",boxSizing:"border-box",padding:"9px 12px",borderRadius:10,border:`2px solid ${T.accent}18`,background:"rgba(0,0,0,0.25)",color:T.text,fontFamily:"'Outfit',sans-serif",fontSize:17,fontWeight:700,letterSpacing:3,outline:"none",textTransform:"uppercase"}}/>
              <div style={{fontSize:9,color:T.textDim,marginTop:3}}>Type two words that use all 7 dice letters combined</div>
            </div>
          </div>
        )}
        <div style={{display:"flex",gap:8,alignItems:"center",flexWrap:"wrap"}}>
          <button onClick={handleValidate} disabled={validating||(!twoForOneMode&&keepLine.length===0)} style={{padding:"8px 20px",borderRadius:10,border:"none",background:(validating||(!twoForOneMode&&keepLine.length===0))?"rgba(86,216,156,0.3)":T.success,color:"#0a2015",fontWeight:700,fontSize:13,cursor:(validating||(!twoForOneMode&&keepLine.length===0))?"default":"pointer"}}>{validating?"Checking...":"CHECK WORD"}</button>
          {categories.some(c=>c.type==="twoforone"&&!scores[c.id])&&(
            <button onClick={()=>{setTwoForOneMode(!twoForOneMode);setWord("");setWord2("");clearValidation();}}
              style={{padding:"7px 12px",borderRadius:10,border:twoForOneMode?`1px solid ${T.medium}50`:"1px solid rgba(255,255,255,0.08)",background:twoForOneMode?`${T.medium}15`:"transparent",color:twoForOneMode?T.medium:T.textDim,fontSize:11,fontWeight:600,cursor:"pointer"}}>
              {twoForOneMode?"✓ Two-for-One":"Two-for-One"}
            </button>
          )}
        </div>
        {message&&<div style={{marginTop:7,fontSize:12,color:T.medium,fontWeight:500}}>{message}</div>}
        {validationResult&&Object.keys(eligible).length>0&&(<div style={{marginTop:7,fontSize:11,color:T.success,animation:"slideUp 0.3s ease-out"}}>✓ "{validationResult.word}" is valid!{validationResult.pos.length>0&&` (${validationResult.pos.join(", ")})`} — pick a category:</div>)}
      </div>}
      </div>{/* end wz-roll-arena */}

      {/* Scorecard Toggle */}
      <button className="wz-scorecard-toggle" onClick={()=>setShowScorecard(!showScorecard)} style={{width:"100%",padding:"10px",borderRadius:12,border:`1px solid ${T.accent}20`,background:"rgba(255,255,255,0.015)",color:T.accent,fontFamily:"'Outfit',sans-serif",fontSize:12,fontWeight:700,cursor:"pointer",marginBottom:8,letterSpacing:2,textTransform:"uppercase"}}>
        {showScorecard?"▲ Hide Scorecard":"▼ Show Scorecard"}
      </button>

      {/* Scorecard */}
      {showScorecard&&(
        <div className="wz-scorecard-panel" style={{background:"rgba(255,255,255,0.02)",borderRadius:16,padding:12,border:"1px solid rgba(255,255,255,0.03)"}}>
          {/* Filter tabs */}
          <div style={{display:"flex",gap:4,marginBottom:10,flexWrap:"wrap",justifyContent:"center"}}>
            {[{id:"all",label:"All"},{id:"upper",label:"Upper"},{id:"lower",label:"Lower"},{id:"needed",label:"Needed"},{id:"scored",label:"Scored"}].map(f=>(
              <button key={f.id} onClick={()=>setScorecardFilter(f.id)}
                style={{padding:"5px 12px",borderRadius:8,border:scorecardFilter===f.id?`1px solid ${T.accent}`:"1px solid rgba(255,255,255,0.06)",
                  background:scorecardFilter===f.id?`${T.accent}18`:"transparent",
                  color:scorecardFilter===f.id?T.accent:T.textDim,fontSize:10,fontWeight:700,cursor:"pointer",
                  fontFamily:"'Outfit',sans-serif",letterSpacing:1,textTransform:"uppercase",transition:"all 0.15s"}}>{f.label}</button>
            ))}
          </div>
          {[{label:"Upper Section",filter:c=>c.section==="upper",showSubtotal:true},
            {label:"Lower Section",filter:c=>c.section==="lower",showSubtotal:false}
          ].map(({label,filter:sectionFilter,showSubtotal})=>{
            // Apply scorecard filter
            if(scorecardFilter==="upper"&&label!=="Upper Section")return null;
            if(scorecardFilter==="lower"&&label!=="Lower Section")return null;
            const sectionCats=categories.filter(sectionFilter);
            const filteredCats=sectionCats.filter(cat=>{
              if(scorecardFilter==="needed")return scores[cat.id]===undefined;
              if(scorecardFilter==="scored")return scores[cat.id]!==undefined;
              return true;
            });
            if(filteredCats.length===0&&(scorecardFilter==="needed"||scorecardFilter==="scored"))return null;
            return(
            <div key={label}>
              <div style={{fontSize:9,fontWeight:700,textTransform:"uppercase",letterSpacing:2,color:T.accent,marginBottom:6,marginTop:label==="Lower Section"?14:0,display:"flex",justifyContent:"space-between",alignItems:"center"}}>
                <span>{label}</span>
                {showSubtotal&&<span style={{fontSize:9,color:upperBonus>0?T.success:T.textDim}}>{upperScore}/50{upperBonus>0&&" ✦ +25 BONUS!"}</span>}
              </div>
              {filteredCats.map(cat=>{
                const filled=scores[cat.id]!==undefined;const isElig=eligible[cat.id]!==undefined;
                const typeHints={free:"Any valid word",pos:`Must be a ${cat.pos||""}`,double:"Repeated letter",unique:"No repeated letters",rare:"J, K, Qu, V, X, or Z",palindrome:"Same both ways",compound:"Compound word",wordtzee:"All 7 letters!",twoforone:"2 words, 7 letters"};
                const hint=cat.section==="upper"?`${cat.req} · +${cat.bonus} bonus`:(typeHints[cat.type]||"");
                return(<div key={cat.id} style={{display:"flex",alignItems:"center",justifyContent:"space-between",padding:"8px 10px",marginBottom:2,borderRadius:9,
                  background:isElig?T.successBg:filled?"rgba(255,255,255,0.015)":"transparent",
                  border:isElig?`1px solid ${T.successBorder}`:"1px solid transparent",
                  cursor:isElig?"pointer":filled?"default":"pointer",opacity:filled&&!isElig?0.4:1,transition:"all 0.15s",
                  animation:isElig?"eligiblePulse 1.5s ease-in-out infinite":lastScoredCat===cat.id?"slideUp 0.3s ease-out":"none"}}
                  onClick={()=>isElig?attemptAssign(cat.id):(!filled&&hasRolled?attemptScratch(cat.id):null)}>
                  <div><div style={{fontSize:13,fontWeight:600}}>{cat.name}</div><div style={{fontSize:8,color:T.textFaint}}>{hint}</div></div>
                  <div style={{textAlign:"right"}}>
                    {isElig?(<span style={{fontSize:16,fontWeight:800,color:T.success,animation:"bounceIn 0.3s ease-out"}}>{eligible[cat.id]}</span>)
                    :filled?(<ScoreTooltip breakdown={scores[cat.id]?.breakdown} total={scores[cat.id]?.score}><div style={{cursor:"default"}}><div style={{fontSize:13,fontWeight:700,color:scores[cat.id].score>0?T.text:T.danger}}>{scores[cat.id].score}</div><div style={{fontSize:8,color:T.textFaint}}>{scores[cat.id].word}</div></div></ScoreTooltip>)
                    :(<span style={{fontSize:8,color:T.danger,opacity:0.45}}>✕ scratch</span>)}
                  </div>
                </div>);
              })}
              {/* Upper section subtotal bar */}
              {showSubtotal&&(
                <div style={{margin:"6px 0 0",padding:"8px 10px",borderRadius:8,background:upperBonus>0?"rgba(86,216,156,0.06)":"rgba(255,255,255,0.025)",border:upperBonus>0?`1px solid rgba(86,216,156,0.15)`:"1px solid rgba(255,255,255,0.04)",transition:"all 0.4s"}}>
                  <div style={{display:"flex",justifyContent:"space-between",alignItems:"center",marginBottom:4}}>
                    <span style={{fontSize:10,fontWeight:600,color:upperBonus>0?T.success:T.textDim}}>Subtotal</span>
                    <span style={{fontSize:14,fontWeight:800,color:upperBonus>0?T.success:T.text}}>{upperScore}</span>
                  </div>
                  <div style={{height:5,borderRadius:3,background:upperBonus>0?"rgba(86,216,156,0.15)":"rgba(255,255,255,0.06)",overflow:"hidden"}}>
                    <div style={{height:"100%",borderRadius:3,width:`${Math.min(100,upperScore/50*100)}%`,
                      background:upperBonus>0?`linear-gradient(90deg, ${T.success}, #7ee8b8)`:T.accent,
                      transition:"width 0.4s ease, background 0.4s ease",
                      boxShadow:upperBonus>0?`0 0 10px rgba(86,216,156,0.5)`:""}}/>
                  </div>
                  <div style={{fontSize:9,color:T.textDim,marginTop:3,textAlign:"right"}}>
                    {upperScore>=50?<span style={{color:T.success,fontWeight:700}}>✦ Bonus earned! +25</span>:`${50-upperScore} more for +25 bonus`}
                  </div>
                </div>
              )}
            </div>
          );})}
        </div>
      )}
      </div>{/* end wz-game-layout */}
    </div>
  );
}
