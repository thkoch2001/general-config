#!/bin/sh
for branch in `git branch -r`; do
    if [ `echo $branch | egrep "tags/.+$"` ]; then
        version=`basename $branch`
        subject=`git log -1 --pretty=format:"%s" $branch`
        GIT_COMMITTER_DATE=`git log -1 --pretty=format:"%ci" $branch` \
            git tag -f -m "$subject" "debian/$version" "$branch^"

        git branch -d -r $branch
    fi
done