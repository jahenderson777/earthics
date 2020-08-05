# earthics

Experimental game to increase awareness of climate change.

https://earthics.netlify.app/

## Development Mode

### Start Cider from Emacs:

Refer to the [shadow-cljs Emacs / CIDER documentation](https://shadow-cljs.github.io/docs/UsersGuide.html#cider).

The mentioned `dir-local.el` file has been created.

### Run application:

```
lein dev
```

shadow-cljs will automatically push cljs changes to the browser.

Wait a bit, then browse to [http://localhost:8280](http://localhost:8280).

## Production Build

created with 
lein new re-frame earthics +cider

lein prod
