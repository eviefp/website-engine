# Now

## Under development

Idea: instead of having "posts"/"wiki"/"pages", have common configurable modules

TODO:
- [ ] page
- [ ] wiki
- [ ] tags
- [ ] rss
- [ ] config or API?

Basically the config would look like:
```yaml
base-source: site
base-output: docs
modules:
  - name: index
    output: index.html
    template: template/index.html
    needs:
      - name: post
        kind: module
  - name: static-content
    output: idk lets do it manually first
  - name: post
    output: post/{{post.name}}
    template: template/post.html
```
```js
{
  "source": "site",
  "output": "docs",
  "modules":
    "page": {
      "files": "page//**.md",
      "output": "{{page.url}}", -- allow full mustache
      "template": "template/page.html",
      "tags": {
        "template": "...",
        "output": "...",
        "field": "tags",
      }
    }
  }
}
```

Fields that are assumed to exist:
- id
- title
- content (pseudo-field, the contents of the markdown)
- what about the renderChangelog hack? we probably need an extra field in modules for that

Links should work like [[module:id]] (which gets substituted by title) or [[module:id|foo]]; [[id]] means "current module"

## Future
- Wiki
  - [ ] todo: come back later

- Blog
  - [ ] make wiki links nicer/work within vim as well (currently hacky)
  - [ ] make images take size/positioning hints
  - [ ] categorii (tags for posts, unrelated to tags in wiki)
  - [ ] post date should be nice (today/yesterday/last week, cu tooltip pe hover)

- Homepage
  - [ ] show other posts from insta/TT/YT/masto/??
  - [ ] links to my other stuff

