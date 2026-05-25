---
title: "Perl Heresies: Building Objects Out of Arrays"
author: "Greg Bacon"
date: "1999-03-01"
journal: "The Perl Journal, Spring 1999, Vol. 4, No. 1, Issue 13"
description: "Most people build objects out of hashes. Here's why you shouldn’t."
external_url: https://www.amazon.com/Computer-Science-Perl-Programming-Journal-ebook/dp/B00CUQ00WA?&linkCode=ll2&tag=bloggbaconcom-20&linkId=937c402b761e75dc1c93cee930941142&language=en_US&ref_=as_li_ss_tl
tags:
  - "Perl"
  - "OOP"
---

*Reprinted with the kind permission of The Perl Journal. It also appears in [Computer Science & Perl Programming: Best of The Perl Journal by Jon Orwant][book] from O'Reilly Media, Inc.*

> **Packages Used**  
> Just Perl

Object Oriented Programming is an approach to software design that most programmers simultaneously love and hate. OOP is just so adorable because it provides a simple conceptual model, and code that often looks like natural language:

```perl
my $dog = new Dog; ## create a new dog
$dog->bark; ## Speak, Fido!
```

The misconception that using Object Oriented Design is the proper ritual to drive out bugs and maintenance problems causes most of us to hang our heads and sigh when someone touts the latest Magic Object Oriented Toaster. But the OOP Establishment thrives on rituals.

In the tradition of Galileo, I will challenge the Establishment and boldly state a Perl Heresy:

**Objects in Perl need not be represented with hashes.**

This article assumes at least some knowledge of how to implement objects in Perl. If you don't have that prerequisite, fear not! You are but a short reading of the [perltoot documentation][perltoot] away. This article will explore how using arrays as objects can be a bit more efficient (both timewise and spacewise) and encourage nicer style while refraining from violating the principle in the [perlmodlib documentation][perlmodlib]:

> *Perl doesn't have an infatuation with enforced privacy. It would prefer that you stayed out of its living room because you weren't invited, not because it has a shotgun.*

We'll call this the Graciousness Principle.

# OO Basics

There are several ways to create objects in Perl, but they all have the same effect: They store the state of the world in a data structure and return a reference to it. As the [perltoot] examples show, that data structure can be a scalar, array, hash, typeglob, or even an anonymous subroutine. Each of these has different tradeoffs between flexibility, speed, size, and readability. For whatever reason (as a quick browse of CPAN confirms), Perl programmers prefer hashes as their storage of choice, probably because of hashes' high score on the flexibility scale.

Let's say we want to create some objects to represent people. In OO parlance, we would say that we want to create instances of the Person class. Think of a class as a definition of what objects "know" about themselves. For example, every person knows his name. If one can say something about the entire class, then that is said to be part of the class definition. Now, let's think of a particular person—Larry Wall for instance. Larry knows his name, and he could tell us if we asked him. We can think of Larry as an instance of class Person.

This is how the non-heretical programmer would implement a Person class:

```perl
package Person;

# create a new Person object whose name is provided in @_
# e.g. my $person = new Person 'Larry';
sub new {
    my $class = shift;
    my $self = {};
    $self->{NAME} = shift;
    bless $self => $class;
}

# A method to get or set the NAME
sub name {
    my $self = shift;
    $self->{NAME} = shift if @_;
    $self->{NAME};
}

1;
```

Before we attack this code, a little terminology. The `new` subroutine above is called a *constructor* because it is invoked when you want to create a new Person object. Subroutines that know how operate on objects are called *methods*. (One might think of addition and subtraction as methods in the class of Integers, for example.) If a method is used only to store and retrieve an attribute (`NAME`, above) of an object, it's called an accessor; `name` is an accessor.

Say we want to have our Person objects come to the microphone and introduce themselves:

```perl
my $lwall = new Person 'Larry';
my $tchrist = new Person 'Tom';

for $person ($lwall, $tchrist) {
    print "Hi, my name is ", $person->name, ".\n";
}
```

As you can see, because of the way we have generalized the notion of what a person is and how to coax information out of people, we can use generic code to make people speak up.

One of the tenets of OO is that you should be able to write code like this independent of how `new` and `name` are defined. When you peer inside the black box of a Person object, you see a hash: the `my $self = {}` makes `$self` into a reference to an anonymous hash, and the [`bless`][bless] at the end of the constructor makes that into an object. Everywhere you look, you'll see constructors like this.

However, if you're willing to write your accessors differently, you can say my `$self = []` and build objects out of arrays. Here's a new `new` that creates an object out of an array:

```perl
sub new {
    my $class = shift;
    my $self = [];
    $self->[NAME] = shift;
    bless $self => $class;
}
```

Our new `name` also just trades braces for brackets:

```perl
sub name {
    my $self = shift;
    $self->[NAME] = shift if @_;
    $self->[NAME];
}
```

There are four reasons why arrays are preferable to hashes:

1. They're faster.
2. They use less space.
3. They prevent attribute collisions.
4. They prevent you from misspelling attribute names.

The first two are general truths, and the last two are specific to a technique for manipulating attributes described later. I'll discuss each of these reasons one by one.

# Arrays are faster

We've only exchanged braces for brackets, but Graham Barr (author of the popular [IO] and [libnet] packages) reported that when he reworked [Convert::BER] to use arrays instead of hashes, he saw a speed improvement of better than twenty percent. To test this claim, I used a base class with two attributes and a subclass with one. The benchmark program used both arrays and hashes to construct new objects and read and write those attributes. My simple benchmarks (available on the TPJ web site) found a comparable speed increase.

Why the increase? Because you can retrieve an element from an array faster than from a hash, although as [perltoot] points out, the savings are not as substantial as one might hope:

> *You might guess that the array access would be a lot faster than the hash access, but they're actually comparable. The array is a little bit faster, but not more than ten or fifteen percent, even when you replace the variables above like `$AGE` with literal numbers, like `1`.*

# Arrays use less space

My benchmark program created large arrays of the subclassed objects. Using the `PERL_DEBUG_MSTATS` feature to measure the space, I found that arrays used a little more space initially, but for large numbers of objects, hashes used about 40% more memory. (All benchmarks ran under Perl 5.005_02.) [perltoot] continues:

> *A bigger difference between the two approaches can be found in memory use. A hash representation takes up more memory than an array representation because you have to allocate memory for the keys as well as for the values.*

Although space and time requirements should not be the standards by which all implementations are judged, they become increasingly important as the application creates more and more objects.

# Arrays can prevent attribute collisions

Let's say that we define a subclass of the Person class: Child. Any Person can have an email address, even children, so we make `EMAIL` an attribute of all Person objects. An `email` accessor is used to get and set the email address. And if we implement our objects as hashes, the attribute would be stored as `$object->{EMAIL}`. So far, so good.

The sharp reader will wonder what prevents a subclass from inadvertently accessing the attribute of a superclass. Envision a collaborative software project in which different people implement different classes. The author of the Child class didn't remember that the author of the Person class implemented an `EMAIL` attribute, and in a late night coding frenzy he used `$object->{EMAIL}` as a boolean to keep track of whether or not the Child is old enough to read email. That's a *collision*, and it's a common bug in large software projects.

Unfortunately, there's no easy way to prevent collisions, and this is one of the biggest problems with hashes as objects. It might be possible to represent the object with a tied hash whose `FETCH` and `STORE` methods (see the [perltie documentation][perltie]) maintained a registry of which classes are allowed to access which keys, but this would be very cumbersome and slow. Such a solution would violate the Graciousness Principle.

We're going to solve the collision problem by ensuring that each attribute is assigned a unique number. That shouldn't come as a surprise; the big difference between hashes and arrays is that hashes let you name elements, while arrays only let you number them. Naming is more convenient and intuitive—but as you'll see, there's a clever workaround that allows us to name the elements of our array and avoid collisions.

The [perlsub documentation][perlsub] describes how you can create constants at compile time. Here's the common example:

```perl
sub PI () { 3.14159 } # define constant PI
use constant PI => 3.14159; # has the same effect
```

Subroutines like this are a hint to the Perl compiler that it's okay to inline the subroutine's return value—to substitute `3.14159` wherever it sees `PI`, just like the C preprocessor.

We can use this for our attributes, converting names into numbers with subroutines like `sub NAME { 0 }` and `sub EMAIL { 1 }`. However, we don't want to force everyone in a large software project to agree on which numbers to use, because inevitably people will forget—just like our late-night hacker forgot about the `EMAIL` attribute—and collisions will occur.

So what we'll do is generate those numbers automatically. Our collision protection for root classes (that is, classes with no superclasses) goes near the top of the class:

```perl
package Person;

my @Attributes;
BEGIN {   # executed at compile time
    @Attributes = qw( NAME EMAIL );

    my $i = 0;
    for (@Attributes) {
        eval "sub $_ () { $i }";
        $i++;
    }
}

sub ATTRIBUTES { @Attributes }
```

The Child subclass looks like this:

```perl
package Child;

use Person;

my @Attributes;
BEGIN {
    @Child::ISA = qw( Person );
    @Attributes = qw( PARENT EMAIL );

    my $class = 'Child';

    # Set $i to the number of known attributes,
    # which is also the next free index in the array.
    my $i = $class->SUPER::ATTRIBUTES;

    for (@Attributes) {
        eval "sub $_ () { $i }";
        $i++;
    }
}

sub ATTRIBUTES {
    my $class = shift;
    my @a = ($class->SUPER::ATTRIBUTES, @Attributes);
    return @a;
}

sub parent {
    my $self = shift;
    $self->[PARENT] = shift if @_;
    $self->[PARENT];
}

1;
```

The purpose of all this `ATTRIBUTE` code is to ensure that every attribute throughout the class hierarchy has a unique integer. That integer corresponds to its index in the array, which is how we can get away with the illusion of having conveniently named attributes instead of unintuitive integers. Thanks to the constants created by our [`eval`][eval] statement, it's speedy. We even observe the Graciousness Principle: anyone can access the array directly, or even recover the symbol associated with a particular index.

# Arrays can prevent misspellings

If you were really determined to use hashes, you could do what Graham Barr did to avoid collisions and always put the class name in the keys to avoid ambiguity. So a Child's email address would be `$obj->{CHILD_EMAIL}`. This works, but it's cumbersome and a lot of extra typing. Furthermore, deep subclasses will have extremely long keys like `Person_Worker_Blue_Collar_Construction`. The longer the name, the more opportunities to misspell it.

Our shortened attribute names are simpler to spell. And there's an added bonus: If our class uses the [`strict` pragma][strict], you'll be notified at compile time if you misspell a key!

# Disadvantages

This technique for collision avoidance won't work for multiple inheritance. Depending on your opinion of MI, this is either a Good Thing or a Big Loss. Given that forcing people to do things isn't nice, I wish I knew of a way to reconcile this approach with multiple inheritance.

Furthermore, this approach also requires that all attributes be known in advance; you can't add more attributes at run time. If you do, you run the risk of two attributes being awarded the same index number, causing collisions.

The biggest disadvantage to this approach is the inertia that the hash representation has gained over the past few years. One criticism of Perl's OO is that subclasses are forced to use the representation of the superclass. Using the array representation described here means retrofitting lots of old code, and the path of least resistance dictates that people will happily stay where they are. Of course, that doesn't stop anyone from using this approach in new code, and I hope they will, because it makes the right tradeoffs between flexibility, speed, size, and readability.

# Other Approaches

As the number of elements in an array grows large, the space costs become more and more expensive. When it becomes untenable, we could store our arrays in a packed string, or even a simple string like `$obj = join $;, @array;`. Some details are hazy; for instance, how do you assign to the middle of your scalar efficiently? Repeatedly inflating and deflating the object would probably kill any gain. However, if the object is mostly read-only, it might be a reasonable tradeoff.

Another approach is to use a new experimental feature of post-5.005 Perls known as [*pseudo-hashes*][phash] [which were removed as of version 5.10]. Pseudo-hashes create mappings from names to indices for accessing data, but instead of encoding the mapping with subroutines, each pseudo-hash carries a reference to a hash that associates names with indices. They are more general than the technique I've described here, but that generality costs space and time. You can find the documentation on pseudo-hashes in the [perlref] and [fields] documentation.

This new approach to implementing objects illustrates that you don't have to do what everyone else does to do your job well. When you can build your objects out of arrays, you benefit from a cleaner style, less memory use, less access time, attribute spell-checking, and collision protection. If arrays aren't feasible, remember that There's More Than One Way To Do It, and take advantage of Perl's empowering flexibility to craft a solution that works for you.

---

*Greg Bacon is a system administrator and software developer for the [Information Technology and Systems Center][itsc] at the University of Alabama in Huntsville.*

[bless]: https://perldoc.perl.org/functions/bless
[book]: https://www.amazon.com/Computer-Science-Perl-Programming-Journal-ebook/dp/B00CUQ00WA?&linkCode=ll2&tag=bloggbaconcom-20&linkId=937c402b761e75dc1c93cee930941142&language=en_US&ref_=as_li_ss_tl
[Convert::BER]: https://metacpan.org/dist/Convert-BER
[eval]: https://perldoc.perl.org/functions/eval
[fields]: https://perldoc.perl.org/fields
[IO]: https://perldoc.perl.org/IO
[itsc]: https://www.itsc.uah.edu/
[libnet]: https://metacpan.org/dist/libnet
[perlmodlib]: https://perldoc.perl.org/perlmodlib
[perlref]: https://perldoc.perl.org/perlref
[perlsub]: https://perldoc.perl.org/perlsub
[perltie]: https://perldoc.perl.org/perltie
[perltoot]: https://perldoc.perl.org/perltoot
[phash]: https://perldoc.perl.org/perlref#Pseudo-hashes:-Using-an-array-as-a-hash
[strict]: https://perldoc.perl.org/strict
