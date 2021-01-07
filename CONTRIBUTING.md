# Contributing

When contributing to this repository, please first discuss the change you wish
to make via issue, email, or any other method with the owners of this repository
before making a change.

If you are new to contributing then please start with "Issues" available on the
website.

Please note we have a code of conduct, please follow it in all your interactions
with the project.

## Branches

There are two permanent branches.

1. `master`. This branch is the most stable production one.

2. `develop`. This branch is where actual development takes place.

## Tagging

All tags start with `v`.
Examples: `v3.2.1`.

The versioning scheme we use is [SemVer](http://semver.org/).

## Merge Request Process

1. Ensure that no build files are uploaded and if possible update the
gitignore file.
2. Update the README.md with details of changes to the interface, this includes
new environment variables, exposed ports, useful file locations and container
parameters.
3. Increase the version numbers in any examples files and the README.md to the
new version that this. Merge Request would represent.
4. You may accept the Merge Request in once you have the sign-off of two other
developers, or if you do not have permission to do that, you may request the
second reviewer to merge it for you.
5. All Merge Request needs to be labeled appropriately. Check the [Labelling method].
6. All Merge Request must also be attached to a milestones.

## Branching method

We use Git flow branching method. [Read more about this](https://www.atlassian.com/git/tutorials/comparing-workflows/gitflow-workflow).

For macOS users `brew install git-glow` should work, provided you have Homebrew
installed.
To install Homebrew follow these [instructions](https://brew.sh).

2) Run `git flow init` in your project root.

Accept all the defaults:
```
Branch name for production releases: [master]
Branch name for "next release" development: [develop]
Feature branches: [feature/]
Release branches: [release/]
Hotfix branches: [hotfix/]
Support branches: [support/]
Version tag prefix: []
```

Instructions to use git-flow:

1) Creating a feature branch
```
git flow feature start feature_branch
```

2) Finishing a feature branch
```
git flow feature finish feature_branch
```

3) Creating a release branch
```
git flow release start v0.1.0
```

4) Merging a release branch into master and develop
```
git checkout master
git checkout merge release/v0.1.0
git flow release finish 'v0.1.0'
```

5) Creating a hotfix branch
```
git flow hotfix start hotfix_branch
```

6) Finishing a hotfix branch
```
git flow hotfix finish hotfix_branch
```

## Creating a new Issue

Please use issues as much as possible. Issues are the best way to communicate
about the project. Don't email the project maintainers unless necessary.

Please use the template. Every issue must have correct labels, milestone
information.

For Bug reports one issue per bug. If multiple bugs are related then use related
feature on the website or add the url to the related bug in the Description.

### Labelling method
All issues are categorised into three:

1. **Priority**
    1. ~"Priority: Critical"
    2. ~"Priority: High"
    3. ~"Priority: Medium"
    4. ~"Priority: Low"

2. **Type**
    1. ~"Type: Bug"
    2. ~"Type: Maintenance"
    3. ~"Type: Enhancement"
    4. ~"Type: Discussion"

3. **Status**
    1. ~"Status: To Do"
    2. ~"Status: In Progress"
    3. ~"Status: In Review"

[Labelling method]: (#labelling-method)
