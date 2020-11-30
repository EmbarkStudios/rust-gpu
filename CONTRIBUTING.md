# Embark Contributor Guidelines

Welcome! This project is created by the team at [Embark Studios](https://embark.games). We're glad you're interested in contributing! We welcome contributions from people of all backgrounds who are interested in making great software with us.

At Embark, we aspire to empower everyone to create interactive experiences. To do this, we're exploring and pushing the boundaries of new technologies, and sharing our learnings with the open source community.

If you have ideas for collaboration, email us at opensource@embark-studios.com.

We're also hiring full-time engineers to work with us in Stockholm! Check out our current job postings [here](https://www.embark-studios.com/jobs).

## Issues

### Feature Requests

If you have ideas or how to improve our projects, you can suggest features by opening a GitHub issue. Make sure to include details about the feature or change, and describe any uses cases it would enable.

Feature requests will be tagged as `enhancement` and their status will be updated in the comments of the issue.

### Bugs

When reporting a bug or unexpected behaviour in a project, make sure your issue describes steps to reproduce the behaviour, including the platform you were using, what steps you took, and any error messages.

Reproducible bugs will be tagged as `bug` and their status will be updated in the comments of the issue.

### Wontfix

Issues will be closed and tagged as `wontfix` if we decide that we do not wish to implement it, usually due to being misaligned with the project vision or out of scope. We will comment on the issue with more detailed reasoning.

### Labels
The labels for this repository are divided into the following categories;

- **`c:` Crate** Issues specific a single crate in the repository.
- **`g:` GPU** Issues specific a GPU vendor.
- **`p:` Platform** Issues specific a single operating system or platform.
- **`s:` Status** The current status of a PR or issue.
- **`t:` Type** The general type of the issue. (E.g. `t: bug` for bugs.)

## Contribution Workflow

### Open Issues

If you're ready to contribute, start by looking at our open issues tagged as [`help wanted`](../../issues?q=is%3Aopen+is%3Aissue+label%3A"help+wanted") or [`good first issue`](../../issues?q=is%3Aopen+is%3Aissue+label%3A"good+first+issue").

You can comment on the issue to let others know you're interested in working on it or to ask questions.

### Major Change Process
Most bug fixes can be implemented directly by opening a PR, however for larger design decisions and major changes to the compiler's architecture, this repository uses a two stage "Major Change Proposal" and "Request For Comments" process. If you're unsure about what's required for a specific change you should always start with [**opening an issue**][open-issue] or asking the team over on the `#rust-gpu` channel in the [Embark Discord][dis].

[dis]: https://discord.gg/8TW9nfF
[open-issue]: https://github.com/EmbarkStudios/rust-gpu/issues/new

#### Definitions

##### **Major Change Proposal (MCP)**
A proposal to make a significant internal changes or small public facing changes to the compiler. An MCP is opened as an issue on `rust-gpu` the repository. An MCP typically only requires one member's approval. Though if the change is significantly big enough it may require the full team's sign off or require an RFC.

  An MCP should generally be a short (1-2 paragraphs) high level overview of the change you would want to make, the motivation behind the change, and potential solutions. There is a [major change issue template][mcp-template] you can use for convenience.

[mcp-template]: https://github.com/rust-lang/rust/issues/new?labels=mcp%3A%20proposed&template=mcp.md

  Examples of what would require an MCP:

  - Changing the compiler architecture.
  - Adding support for an existing Rust language or feature.
  - Small additions (e.g. new methods) to `spirv-std` types.
  - Proposing an RFC.

  Examples of what would **not** require an MCP:

  - Updating documentation
  - Fixing existing bugs
  - Performance improvements

##### **Request For Comments (RFC)**
A proposal to make significant public facing changes to the compiler or standard library. RFCs are opened as pull requests to the `rust-gpu` repository. RFCs require full sign off by the team, before being approved or implemented. Check out the [RFC `000-template.md` document][rfc-template] for details on the structure.

[rfc-template]: https://github.com/EmbarkStudios/rust-gpu/blob/main/rfcs/000-template.md

Examples of what would require an RFC:

- Major additions to `spirv-std`, such as new APIs, or breaking changes to existing ones.

#### Life-cycle

1. You file a [major change proposal][mcp-template] outlining the changes and the motivation for it.
2. A member of the team will review the proposal and tag it with the appropriate label.
  2.1. `mcp: accepted` means that the MCP has been accepted and is ready for a pull request implementing it.
  2.2. `mcp: rfc needed` means that the MCP has been accepted as something the team would like but needs a full RFC before the implementation.
  2.3 Closing an issue means that the MCP has rejected.
3. If the proposal has been accepted then the implementation can begin.

### Pull Request Process

1. Fork the repository.
2. Create a new feature branch.
3. Make your changes. Ensure that there are no build errors by running the project with your changes locally.
4. Open a pull request with a name and description of what you did. You can read more about working with pull requests on GitHub [here](https://help.github.com/en/articles/creating-a-pull-request-from-a-fork).
5. A maintainer will review your pull request and may ask you to make changes.

## Code Guidelines

### Rust

You can read about our standards and recommendations for working with Rust [here](https://github.com/EmbarkStudios/rust-ecosystem/blob/master/guidelines.md).

## Licensing

Unless otherwise specified, all Embark open source projects are licensed under a dual MIT OR Apache-2.0 license, allowing licensees to chose either at their option. You can read more in each project's respective README.

## Code of Conduct

Please note that our projects are released with a [Contributor Code of Conduct](CODE_OF_CONDUCT.md) to ensure that they are welcoming places for everyone to contribute. By participating in any Embark open source project, you agree to abide by these terms.
