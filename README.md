selector-util-jvm
=================

A JVM fork of selector-util

API
=======

##`class Selector`##

###`public static SelectorsOpt apply(string selctorSource)`###
return a `SelectorsOpt` instance.


##`class SelectorsOpt`##

###`public boolean contains(string selctorSource)`###
decide whether selectorSource is a sub-selector of `SelectorsOpt`


*By saying that A is `sub-selector` of B, it means in every dom, the set of elements matching A is a subset of that matching B*

Thread Safety
====

The code base has no synchronization or lock.
However, it does not have mutable states nor writing operations.
It is thread safe as long as library users keep reading operations safe.
