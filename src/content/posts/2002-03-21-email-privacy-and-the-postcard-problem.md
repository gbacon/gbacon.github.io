---
title: "Email privacy and the postcard problem"
author: "Greg Bacon"
date: "2026-09-14"
description: "If the Post Office delivered only postcards, you’d encrypt your mail—so why treat email like an open card?"
tags:
  - "Exponent"
  - "UAH"
  - "Tech Talk"
---

*This Tech Talk column originally appeared in The Exponent (University of Alabama in Huntsville), Vol. 33, No. 25 (March 21, 2002). Digitized issue: [UAH LOUIS archive](https://louis.uah.edu/exponent-2002/25). Reproduced here with courtesy to that archive.*

*This column also appeared (as a reprint) on October 19, 2000; both appearances are kept as separate posts.*

What if the U.S. Postal Service decreed that it would only deliver postcards after today? Such a decision wouldn’t be as catastrophic as it might seem. You could still use the mail to pay your bills: “Please charge my phone bill to credit card XXXX-XXXX-XXXX-XXXX.” You’d have to attach a check to your credit card bill or maybe play balance-transfer roulette. Your employer could use the same method to send your paycheck.

Sound good? Tampering with the mail is a federal crime, so people don’t really need envelopes. Fine, you’re disqualified from ever being Postmaster. Such a scenario would be unacceptable to any sensible person! Who would write his credit card number on a postcard for anyone to steal? Who would want his paycheck and Social Security number out in the open for some nosy cretin to steal? Stealing is wrong, but property owners still have the responsibility to lock their doors. (That leads to the saying that locks only keep honest people out.)

There is a point to the postcard analogy. Every time you send email, it’s as if you’vemailed a postcard with no protection from prying eyes. For those who would dismiss such concerns as paranoia, consider the FBI’s Carnivore, a system in use now for eavesdropping on email correspondence (see [http://www.stopcarnivore.org/](http://www.stopcarnivore.org/)). Wiretapping is supposed to require a court order, but that requirement is easy to circumvent in an environment like the Internet where information scurries unprotected from one place to another.

Despite the common perception, email is a form of written communication and is different in the eyes of the legal system than, say, a spoken conversation. In other words, old email can come back to haunt you.

A perfect example is Jamie Zawinski, former employee of Netscape Communications. He ran a mailing list at Netscape called really-bad-attitude that he and his coworkers used to vent their frustrations associated with work. Jamie promised that he would keep the list’s contents private, but Microsoft subpoenaed the contents of the list during the antitrust trial. It was a huge embarrassment to Netscape and especially to the people whose once-private grumblings had become matters of public record.

There are several solutions to the problem. One is to avoid transmitting any information over a computer network that you wouldn’t post on a bulletin board. That’s fine as long as you don’t mind sacrificing many conveniences. For example, UAH’s online registration system uses student ID numbers (which, irritatingly, are usually Social Security numbers), and eschewing the online registration system would mean going back to standing in line for an hour only to find out that the class you needed to graduate is full.

Another solution is to protect the information in transit. There are lots of systems currently in place that protect information. For example, Secure Sockets Layer (SSL) is technology that web browsers incorporate to secure WWW transactions. There’s a utility called Secure Shell that is a drop-in replacement for telnet that secures connections between machines on a network. The GNU Privacy Guard, `GPG` (see [http://www.gnupg.org/](http://www.gnupg.org/)), is free software that encrypts the text of email messages so that only your intended recipient can read it.

Of course, encryption isn’t a panacea. You have to trust the person who receives the email, and you may still be compelled to reveal your passphrase. You might go to jail for contempt of court, but the information will still be protected (allowing you to stick a finger in the government’s eye). The government, of course, has its finger in the wind and has proposed privacy-invasion schemes like key escrow where everyone using encryption has to file a key with the government “just in case.” (Imagine having to provide the government with a key to your car “just in case.”)

As usual, the issue of privacy depends on citizens being savvy enough to realize when government is trying to erode those rights. I’ll close with a Franklin quote: “They that can give up essential liberty to obtain a little temporary safety deserve neither liberty nor safety.”
