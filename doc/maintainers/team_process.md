<div class="hidden-warning"><a href="https://docs.haskellstack.org/"><img src="https://cdn.jsdelivr.net/gh/commercialhaskell/stack/doc/img/hidden-warning.svg"></a></div>

# Stack Maintainer Team Process

## Purpose of this document

This guide is intended to formalize the process by which the `Stack` maintainer
team operates. We expect it will evolve over time as experience makes clearer
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
issues and pull requests. From experience, we find it is easiest to have a
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
it would be a valuable improvement, whereas it is not so clear to the triager.

Discerning which requests should be considered "small" and which warrant
broader collaboration is admittedly an inexact science. Use your best judgment
and do not sweat the occasional mistake if you approve something you thought was
small but ultimately was not.

In the case of medium-to-large sized feature requests, it is best to solicit
feedback from at least one or two of the core `Stack` developers. You may use
GitHub @mentions to draw the relevant contributors' attention to the issue in
question. If you are not sure who's best to consult you should ask on
`#stack-collaborators`.

Try to be clear to the requester that you are opening the discussion up to more
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
called [`#stack-collaborators`](../CONTRIBUTING.md#slack-channel) in the
Haskell Foundation's Slack workspace, although we may migrate to some other
platform in the future.

## Dealing with support issues

We encourage users to submit support requests via GitHub issues and try to
consolidate our efforts in addressing questions there. Other platforms, such as
Reddit, StackOverflow, et al, may be helpful as well but the `Stack`
maintainers team makes no guarantees about regularly checking them.

*We may decide to offload support questions elsewhere in the future, but for
now the most important thing is to direct users to a single destination.*

## Issue vs pull request

Sometimes it is ambiguous whether something should be opened as an issue to
discuss a change, versus a pull request including the change. A guiding
principle here is: if it will take longer to open an issue than a PR, open the
PR. As an extreme example: if you have a small documentation typo correction,
open a PR, it will _definitely_ take less time than opening an issue and
describing the change you'd like to make. This logic can sometimes apply to
minor code changes. Use your best judgement.

## Issue closing policy

We need to strike a balance in issue handling between keeping a maintainable
set of issues&mdash;thereby making the lives of maintainers, contributors, and
users easier&mdash;and allowing legitimate issues to remain open. The end goal
of this is: any open issue indicates "this deserves attention in the near
future." Before this policy existed, issues actually meant "maybe someone will
deal with this someday."

The policy for closing an issue depends entirely on the type of issue we are
looking at.

When closing an issue, please provide a brief explanation for why the issue was
closed, such as a reference to a PR, a comment about lack of clarification,
etc.

!!! note

    The following sections establish rules under which a bug report will be
    closed by the maintainer team. The goal is to avoid a situation where issues
    linger in an indeterminate state. The maintainer team is allowed to
    disregard these "rules" at any point. In other words: the goal is not to
    allow people to "lawyer" issues.

### Discussion

Some issues are for discussing the project. These issues should start with a
clear question, have an owner, and be closed when either:

* The original question has been answered
* There is no clear question or owner
* No progress has been made in 30 days

Discussions are typically opened by maintainers and contributors.

### Support

Support questions are requests for assistance in using Stack. They must have a
clear set of steps followed by the user and a desired outcome. Support requests
sometimes overlap with bug reports, handled next. Support requests are closed
when:

* The user decides their question is answered
* No clarification to questions from the Stack team is given for 30 days
* The question is determined out of scope by the Stack team

### Bug report

A bug report describes a misbehavior in Stack itself. There are some cases
where an issue may appear somewhere between a support request and a bug report.
The Stack team will determine which category an issue falls into. Bug reports
must include clear reproduction steps, an expected outcome, and an actual
outcome. Bug reports are closed when:

* The bug is fixed on the `master` or `stable` branch (NOTE: we do not wait for
  fixes to be released before closing issues)
* The Stack team is unable to reproduce the issue
* No clarification to questions from the Stack team is given for 30 days
* The bug is determined to be out of scope by the Stack team, such as being an
  upstream issue

### Feature request

Feature requests are usually clear, but sometimes have overlap with one of the
above topics. Determination of an issue being a feature request will be made by
the Stack team. Features are closed when:

* The feature is implemented on the `master` or `stable` branch
* The Stack team decides that the feature is not desired in the project
* If no one offers to implement the feature for 30 days

Note that, to account for the last bullet, we have a
[Wishlist](https://github.com/commercialhaskell/stack/wiki/Wishlist) wiki page. We
recommend closing issues with a message such as the following:

> Closing due to lack of implementation progress in the past 30 days. We
> attempt to keep the Stack issue tracker focused on active work items. If you
> are still interested in this feature and would like to see it implemented in
> the future, please add it to the
> [Wishlist](https://github.com/commercialhaskell/stack/wiki/Wishlist) Wiki page.
