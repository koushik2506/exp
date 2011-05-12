-module(yaml_test).
-author("Oliver Mason").
-export([test/0]).

% Test cases taken from http://yaml4r.sourceforge.net/cookbook/
% (YAML for Ruby Cookbook)
test() ->
	do_test("simple sequence",
		["- apple", "- banana", "- carrot"],
		["apple", "banana", "carrot"]),
	do_test("nested sequence",
		[ "-", "  - foo", "  - bar", "  - baz"],
		[["foo","bar","baz"]]),
	do_test("mixed sequences",
		["- apple", "-", "  - foo", "  - bar", "  - x123", "- banana", "- carrot"],
		["apple", ["foo","bar","x123"],"banana","carrot"]),
	do_test("deeply nested sequences",
		[ "-", " -", "  - uno", "  - dos"],
		[[["uno","dos"]]]),
	do_test("simple mapping",
		[ "foo: whatever", "bar: stuff"],
		[{"foo", "whatever"}, {"bar", "stuff"}]),
	do_test("sequence in a mapping",
		[ "foo: whatever", "bar:", " - uno", " - dos"],
		[{"foo","whatever"},{"bar", ["uno","dos"]}]),
	do_test("nested mappings",
		[ "foo: whatever", "bar:", " fruit: apple"," name: steve"," sport: baseball"],
		[{"foo","whatever"},{"bar",[{"fruit","apple"},{"name","steve"},{"sport","baseball"}]}]),
	do_test("mixed mapping",
		[ "foo: whatever", "bar:", " -", "   fruit: apple", "   name: steve",
			"   sport: baseball", " - more", " -", "   python: rocks",
			"   perl: papers", "   ruby: scissorses"],
		[{"foo","whatever"},{"bar", [[{"fruit","apple"},{"name","steve"},
			{"sport","baseball"}],"more", [{"python","rocks"},{"perl","papers"},
			{"ruby","scissorses"}]]}]),
	ok.

do_test(Name,Yaml,Target) ->
	YamlExpr = yaml:parse(Yaml),
	eval_test(Name, YamlExpr, Target).

eval_test(Name, Yaml, Yaml) ->
	io:format("PASS: ~p~n",[Name]);
eval_test(Name, Yaml, Target) ->
	io:format("FAIL: ~p -- got ~p, expected ~p~n",[Name,Yaml,Target]).
