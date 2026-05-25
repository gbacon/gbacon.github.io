---
title: "Just for you, Madeline"
description: "Practice for our newest little reader."
author: "Greg Bacon"
date: "2009-07-11"
tags:
  - "ActionScript"
---
<div id="flash-container" style="width: 400px; height: 250px;"></div>

<script src="https://unpkg.com/@ruffle-rs/ruffle"></script>

<script>
  window.RufflePlayer = window.RufflePlayer || {};

  window.RufflePlayer.config = {
    autoplay: "on",
    unmuteOverlay: "hidden",
  };

  function initRuffle() {
    if (window.RufflePlayer.newest) {
      const ruffle = window.RufflePlayer.newest();
      const player = ruffle.createPlayer();
      const container = document.getElementById("flash-container");
      container.appendChild(player);
      player.style.width = "100%";
      player.style.height = "100%";
      player.load("/words.swf");
    }
    else {
      setTimeout(initRuffle, 200);
    }
  }

  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", initRuffle);
  }
  else {
    initRuffle();
  }
</script>

My five-year-old daughter is learning to read. I made flash cards for us to practice phonics and recognition, and I wrote this simple app to give her a way to practice on her own too. Thanks to the [Wiktionary](http://en.wiktionary.org/) folks for the pronunciations.

Along with the buttons, you can advance by pressing Enter or Right-Arrow and hear the word with S or space bar.

The [code is available on GitHub](http://github.com/gbacon/learn-words).
