---
name: Feature or method request
about: A method, an output, or a design the package should support
title: ''
labels: enhancement
assignees: ''
---

**The question you are trying to answer**

<!-- The analysis, not the function. "I have repeated measures and want the
within-subject effect" is more useful than "add lme4 support". -->

**Your data**

<!-- How many subjects, how many blocks, how many variables per block, the
outcome type (binary, continuous, time-to-event), and whether it is
cross-sectional or longitudinal. -->

**If you are proposing an evidence generator**

<!-- The package merges generators into one scored graph, and the score
weights agreement between them. For that, a generator has to report per
relationship:

  - an estimate, its standard error, its sample size
  - which identification strategy would license reading it causally
  - what it assumes, in a form summary() can print to a clinician

A method that returns only a ranking is welcome as a discussion, but cannot
be merged into the graph as it stands. -->

**Reference**

<!-- Paper, or an existing R implementation. -->
