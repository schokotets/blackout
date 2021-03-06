const Koa = require("koa")
const app = new Koa()
const fs = require("fs")
const path = require("path")

const bodyParser = require("koa-bodyparser")
app.use(bodyParser({
  enableTypes: ["json"]
}))

const serve = require('koa-static')
app.use(serve("../build"))

app.use(async ctx => {
    if (ctx.path == "/document") {
      let doc = ctx.request.query.doc
      if (!doc) return
      if (ctx.method == "POST") {
        save(doc, ctx.request.body)
        ctx.body = "OK"
      } else if (ctx.method == "GET") {
        let doclist = JSON.parse(loadFileList()).documents

        let doctext = load(doc)
        let docinfo = JSON.parse(doctext)

        let matchdocs = doclist.filter(e => e.id == doc)
        if (matchdocs.length >= 1) {
          let index = doclist.indexOf(matchdocs[0])
          docinfo.prev = (index == 0) ? "" : doclist[index-1].id
          docinfo.next = (index == doclist.length - 1) ? "" : doclist[index+1].id
          docinfo.url = matchdocs[0].url
          docinfo.name = matchdocs[0].name
          if (! ("boxes" in docinfo)) {
            docinfo.boxes = []
          }
          ctx.body = JSON.stringify(docinfo)
        } else {
          return docinfo
        }
      } else {
        ctx.throw(405, "method not allowed")
      }
    } else if (ctx.path == "/documentlist") {
      if (ctx.method == "GET") {
        ctx.body = loadFileList()
      } else {
        ctx.throw(405, "method not allowed")
      }
    } else {
      ctx.throw(404, "page not found")
    }
})

app.listen(8090)
console.log("blackout backend listening on :8090")

function save(doc, data) {
  let filepath = path.join("data",doc+".json")
  fs.writeFile(filepath, JSON.stringify(data), function (err,data) {
    if (err) {
      return console.log(err);
    }
    if (data) {
      console.log(data)
    }
  })
  console.log("written to " + filepath)
}

function load(doc) {
  try {
    if (!/^\d+$/.test(doc)) {
      throw new Error("invalid document id: may only contain digits")
    }
    const data = fs.readFileSync(path.join("data",doc+".json"), 'utf8')
    console.log(data)
    return data
  } catch (err) {
    console.error(err)
    return "{}"
  }
}


function loadFileList() {
  return load("documents")
}
