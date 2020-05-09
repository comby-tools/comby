/*
 * mod_fortune - fortune cookies for everyone
 *
 */

#include <lighttpd/base.h>

LI_API gboolean mod_fortune_init(liModules *mods, liModule *mod);
LI_API gboolean mod_fortune_free(liModules *mods, liModule *mod);

/* globals */
typedef struct fortune_data fortune_data;

struct fortune_data {
	GRand *rand;
	GPtrArray *cookies;
};

static GString *fortune_rand(fortune_data *fd) {
	guint r = g_rand_int_range(fd->rand, 0, fd->cookies->len);
	return g_ptr_array_index(fd->cookies, r);
}

static liHandlerResult fortune_header_handle(liVRequest *vr, gpointer param, gpointer *context) {
	fortune_data *fd = param;

	UNUSED(context);

	if (fd->cookies->len) {
		GString *cookie = fortune_rand(fd);
		li_http_header_insert(vr->response.headers, CONST_STR_LEN("X-fortune"), GSTR_LEN(cookie));
	}
	return LI_HANDLER_GO_ON;
}

static liAction* fortune_header(liServer *srv, liWorker *wrk, liPlugin* p, liValue *val, gpointer userdata) {
	UNUSED(srv); UNUSED(wrk); UNUSED(val); UNUSED(userdata);

	if (!li_value_is_nothing(val)) {
		ERROR(srv, "%s", "fortnue.header doesn't take any arguments");
		return NULL;
	}

	return li_action_new_function(fortune_header_handle, NULL, NULL, p->data);
}

static liHandlerResult fortune_page_handle(liVRequest *vr, gpointer param, gpointer *context) {
	fortune_data *fd = param;
	UNUSED(context);

	switch (vr->request.http_method) {
	case LI_HTTP_METHOD_GET:
	case LI_HTTP_METHOD_HEAD:
		break;
	default:
		return LI_HANDLER_GO_ON;
	}

	if (!li_vrequest_handle_direct(vr))
		return LI_HANDLER_GO_ON;

	vr->response.http_status = 200;

	if (fd->cookies->len) {
		GString *cookie = fortune_rand(fd);
		li_chunkqueue_append_mem(vr->direct_out, GSTR_LEN(cookie));
	} else {
		li_chunkqueue_append_mem(vr->direct_out, CONST_STR_LEN("no cookies in the cookie box"));
	}

	return LI_HANDLER_GO_ON;
}

static liAction* fortune_page(liServer *srv, liWorker *wrk, liPlugin* p, liValue *val, gpointer userdata) {
	UNUSED(srv); UNUSED(wrk); UNUSED(val); UNUSED(userdata);

	if (!li_value_is_nothing(val)) {
		ERROR(srv, "%s", "fortnue.header doesn't take any arguments");
		return NULL;
	}

	return li_action_new_function(fortune_page_handle, NULL, NULL, p->data);
}

static gboolean fortune_load(liServer *srv, liPlugin* p, liValue *val, gpointer userdata) {
	gchar *file;
	GError *err = NULL;
	gchar *data;
	gsize len;
	guint count = 0;
	fortune_data *fd = p->data;
	UNUSED(userdata);

	val = li_value_get_single_argument(val);

	if (LI_VALUE_STRING != li_value_type(val)) {
		ERROR(srv, "fortune.load takes a string as parameter, %s given", val ? li_value_type_string(val) : "none");
		return FALSE;
	}

	file = val->data.string->str;

	if (!g_file_get_contents(file, &data, &len, &err)) {
		ERROR(srv, "could not read fortune file \"%s\". reason: \"%s\" (%d)", file, err->message, err->code);
		g_error_free(err);
		return FALSE;
	}

	/* split lines */
	{
		GString *line = g_string_sized_new(127);
		gchar *cur;
		for (cur = data; *cur; cur++) {
			if (*cur == '\n' && line->len) {
				g_ptr_array_add(fd->cookies, line);
				line = g_string_sized_new(127);
				count++;
			}

			/* we ignore non-safe chars */
			if (*cur < ' ' || *cur > '~')
				continue;

			g_string_append_c(line, *cur);
		}

		g_string_free(line, TRUE);
	}

	g_free(data);

	DEBUG(srv, "loaded %u cookies from file '%s'", count, file);

	return TRUE;
}



static const liPluginOption options[] = {
	{ NULL, 0, 0, NULL }
};

static const liPluginAction actions[] = {
	{ "fortune.header", fortune_header, NULL },
	{ "fortune.page", fortune_page, NULL },

	{ NULL, NULL, NULL }
};

static const liPluginSetup setups[] = {
	{ "fortune.load", fortune_load, NULL },

	{ NULL, NULL, NULL }
};



static void plugin_fortune_free(liServer *srv, liPlugin *p) {
	fortune_data *fd = p->data;
	UNUSED(srv);

	/* free the cookies! */
	for (guint i = 0; i < fd->cookies->len; i++)
		g_string_free(g_ptr_array_index(fd->cookies, i), TRUE);
	g_ptr_array_free(fd->cookies, TRUE);

	g_rand_free(fd->rand);

	g_slice_free(fortune_data, fd);
}

static void plugin_fortune_init(liServer *srv, liPlugin *p, gpointer userdata) {
	fortune_data *fd;
	UNUSED(srv); UNUSED(userdata);

	p->options = options;
	p->actions = actions;
	p->setups = setups;
	p->free = plugin_fortune_free;

	p->data = fd = g_slice_new(fortune_data);

	fd->rand = g_rand_new();
	fd->cookies = g_ptr_array_new();
}


gboolean mod_fortune_init(liModules *mods, liModule *mod) {
	liServer *srv = mods->main;

	MODULE_VERSION_CHECK(mods);

	mod->config = li_plugin_register(srv, "mod_fortune", plugin_fortune_init, NULL);

	if (!mod->config)
		return FALSE;

	return TRUE;
}

gboolean mod_fortune_free(liModules *mods, liModule *mod) {
	if (mod->config)
		li_plugin_free(mods->main, mod->config);

	return TRUE;
}
