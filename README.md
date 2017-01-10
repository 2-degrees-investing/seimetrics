# SEImetrics

Aggregating physical asset ownership and production data from subsidiaries to parent companies.


### Git and GitHub

1. Install Git:
  * ```https://git-scm.com/book/en/v2/Getting-Started-Installing-Git```

2. Clone the online GitHub repository *seimetrics* to your local directory:
  * ```git clone https://github.com/2-degrees-investing/seimetrics.git```
  
3. Check status of local repository:
  * ```git status```

4. Work-flow to commit changes:
  1. Edit a local file
  2. Add it to the commit:
    * ```git add <filename>```
  3. Commit all changes:
    * ```git commit -m "Commit message"```
  4. Send all committed changes to the online repository:
    * ```git push origin master```

5. Show changes of local files that have not been committed or pushed to the online repository:
  * ```git diff```
  
6. Ignore file(s):
  * Go to base Git directory (*seimetrics/*) and add to the file *.git/info/exclude*: ```ignoreMe.txt``` (wild-cards work)
 
7. To update local repository to the newest commit in the online repository:
  * ```git pull```

8. Preview changes before doing a merge
  1. ```git fetch```
  2. ```git diff origin/master``` or for a more elaborate diff ```git difftool origin/master```
  3. ```git pull```


More information:
  * Git: http://rogerdudler.github.io/git-guide/
  * Git markdown: https://help.github.com/articles/basic-writing-and-formatting-syntax/
