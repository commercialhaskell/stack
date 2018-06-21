<div class="hidden-warning"><a href="https://docs.haskellstack.org/"><img src="https://rawgit.com/commercialhaskell/stack/master/doc/img/hidden-warning.svg"></a></div>

# Stack Maintainer Team Process

## Purpose of this document

This guide is intended to formalize the process by which the `Stack` maintainer
team operates. We expect it'll evolve over time as experience makes clearer
what works best in practice.

Having a well-documented set of processes and responsibilities has been
instrumental in effectively maintaining the `Stackage` project across a
distributed team
([see the `Stackage` curators' guide
](https://github.com/commercialhaskell/stackage/blob/master/CURATORS.md))
and we hope to replicate that success here.


## Goals

The goals of the `Stack` maintainer team are to:

- Ensure that incoming bug reports and feature requests are properly triaged,
  answered, and/or escalated.

- Provide timely review of, and feedback on, incoming pull requests.

- Set up clear channels of communication for users to make support requests.

- Keep Stack up-to-date with its upstream dependencies, including new GHC and
  Cabal releases, and libraries Stack depends on.

- Increase the [bus factor](https://en.wikipedia.org/wiki/Bus_factor) of the
  project, encouraging more people to contribute heavily to the codebase and
  take ownership of it.

- Make it easy to improve the documentation and test coverage of the project.

The sections below detail various activities by the `Stack` team to realize
these goals.


## Issue triage

The maintainer team provides ongoing review and responses to newly-filed GitHub
issues and pull requests. From experience, we find it's easiest to have a
single person "on call" at any given time. Therefore, the team rotates shifts
on a weekly basis. The "on call" triager is responsible for:

- Reviewing incoming issues and pull requests on GitHub.

- Answering questions he/she is able to.

- Merging PRs he/she feels comfortable reviewing.

- Closing out irrelevant/misplaced issues.

- Pulling in another maintainer to assist if necessary.

The goal of the triager role is to facilitate continual, smooth progress of
`Stack`'s development and to provide triagers an opportunity to become more
familiar with how the project runs.

This is one path to getting a lot of experience with the codebase, plus great
interaction with the rest of the maintainer team, without necessarily taking on
major coding tasks.


## Time commitment

Someone considering joining the issue triager team may be wondering:

> "How large of a time investment is necessary?"

That’s a great question for anyone considering taking on this role. It’s
frankly somewhat uncertain right now since we’re only getting started, but we
can venture a reasonable guess:

We currently average about one new issue per day. Assuming that each issue
entails roughly ten-to-fifteen minutes of total interaction on the part of the
triager (before passing it off to someone else, answering a support question,
etc) that amounts to about one or two hours spent per week being on-call.

Again, these estimates will evolve over time as we settle into an optimal
process, but for now we anticipate growing the team to about eight members
(which is the size of the `Stackage` team as well), each of whom is likely to
spend about two hours in total on upkeep work every eight weeks. 


## Issue and pull request interaction guidelines

These guidelines apply to all members of the maintainer team.

- Be courteous.

- Avoid leaving issues and PRs in an ambiguous state. By default, when you
  respond to an issue/PR, you’re taking responsibility for shepherding it to
  completion. Be explicit if you’re passing it to someone else, or asking the
  submitter for more info/updates/etc.

- Use labels, assignees, and milestones wherever possible.

- If you know you’ll be unable to answer for a significant period of time, say
  so. Even better: ask someone else to take over from you.


## Assessing feature requests and enhancement PRs

**NB: this section is very likely to evolve over time as well.**

A frequent source of new GitHub issues is users who would like to propose some
enhancement and/or change to how `Stack` currently operates.

Some of these are both small and obviously beneficial, in which case the
triager should feel empowered to "green light" the idea if it makes sense to do
so.

If the proposal is already in the form of an actionable PR the triager may
review and merge at their discretion.

Oftentimes though, the request is larger and more far-reaching in nature.

Perhaps the requester is unaware of the extent to which his or her change would
impact other people's workflows or related components in the code. Conversely,
they may have a deep understanding of its implications and feel strongly that
it would be a valuable improvement, whereas it's not so clear to the triager.

Discerning which requests should be considered "small" and which warrant
broader collaboration is admittedly an inexact science. Use your best judgment
and don't sweat the occasional mistake if you approve something you thought was
small but ultimately wasn't.

In the case of medium-to-large sized feature requests, it's best to solicit
feedback from at least one or two of the core `Stack` developers. You may use
GitHub @mentions to draw the relevant contributors' attention to the issue in
question. If you're not sure who's best to consult you should ask on
`#stack-collaborators`.

Try to be clear to the requester that you're opening the discussion up to more
participants and that the proposal will require thoughtful consideration
(probably a majority vote) before any decision is made. 

Also remember that busy schedules, lack of complete familiarity with a given
subject, strong-yet-opposing opinions held by equally rational people, and many
other factors often collude to halt forward progress in arriving at firm
conclusions in such situations... If at all possible, try to keep things
on-track and concrete.

*Regardless of feature size, bear in mind that sometimes the best answer is a
respectful "no".*

*If feasible, propose alternative solutions or educate the user in preference to
complicating `Stack` or accepting scope-creep.* 

With respect to assessing a PR's code, @snoyberg has outlined some
[tips for successfully getting one's work merged
](https://www.snoyman.com/blog/2017/06/how-to-send-me-a-pull-request)
into his projects which may be helpful to `Stack` triagers when performing
reviews, as well.


## Real-time communications

At present, the maintainer team mostly communicates via a rather quiet channel
called [`#stack-collaborators`](../CONTRIBUTING/#slack-channel) on FP
Complete's Slack workspace, although we may migrate to some other platform in
the near future.


## Dealing with support issues

We encourage users to submit support requests via GitHub issues and try to
consolidate our efforts in addressing questions there. Other platforms, such as
Reddit, StackOverflow, et al, may be helpful as well but the `Stack`
maintainers team makes no guarantees about regularly checking them.

*We may decide to offload support questions elsewhere in the future, but for
now the most important thing is to direct users to a single destination.*


## Old issue closing policy

We have yet to settle on a precise guide for closing out old issues, but, for
now, if you happen across something that could easily be marked as resolved,
please do so.
