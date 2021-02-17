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
        ctx.body = load(doc)
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
    const data = fs.readFileSync(path.join("data",doc+".json"), 'utf8')
    console.log(data)
    return data
  } catch (err) {
    console.error(err)
    return "{}"
  }
}

