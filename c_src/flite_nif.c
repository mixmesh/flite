//
// Flite NIF binding
//
#define _GNU_SOURCE
#include <stdio.h>
#include <stdlib.h>
#include <memory.h>
#include <string.h>
#include <errno.h>
#include <flite/flite.h>

#include "erl_nif.h"

#define MAX_PATH    1024

// #define DEBUG
// #define NIF_TRACE

#define DLOG_DEBUG     7
#define DLOG_INFO      6
#define DLOG_NOTICE    5
#define DLOG_WARNING   4
#define DLOG_ERROR     3
#define DLOG_CRITICAL  2
#define DLOG_ALERT     1
#define DLOG_EMERGENCY 0
#define DLOG_NONE     -1

#ifndef DLOG_DEFAULT
#define DLOG_DEFAULT DLOG_NONE
#endif

#define DLOG(level,file,line,args...) do {				\
	if (((level) == DLOG_EMERGENCY) ||				\
	    ((debug_level >= 0) && ((level) <= debug_level))) {		\
	    emit_log((level),(file),(line),args);			\
	}								\
    } while(0)

#define DEBUGF(args...) DLOG(DLOG_DEBUG,__FILE__,__LINE__,args)
#define INFOF(args...)  DLOG(DLOG_INFO,__FILE__,__LINE__,args)
#define NOTICEF(args...)  DLOG(DLOG_NOTICE,__FILE__,__LINE__,args)
#define WARNINGF(args...)  DLOG(DLOG_WARNING,__FILE__,__LINE__,args)
#define ERRORF(args...)  DLOG(DLOG_ERROR,__FILE__,__LINE__,args)
#define CRITICALF(args...)  DLOG(DLOG_CRITICAL,__FILE__,__LINE__,args)
#define ALERTF(args...)  DLOG(DLOG_ALERT,__FILE__,__LINE__,args)
#define EMERGENCYF(args...)  DLOG(DLOG_EMERGENCY,__FILE__,__LINE__,args)

#ifdef DEBUG
#include <stdio.h>
#define DBG(...) printf(__VA_ARGS__)
#define BADARG(env) printf("matrix_nif.c: badarg line=%d\r\n", __LINE__), enif_make_badarg((env))
#else
#define DBG(...)
#define BADARG(env) enif_make_badarg((env))
#endif

#define ATOM(name) atm_##name

#define DECL_ATOM(name) \
    ERL_NIF_TERM atm_##name = 0

#define LOAD_ATOM(name)			\
    atm_##name = enif_make_atom(env,#name)

#define LOAD_ATOM_STRING(name,string)	\
    atm_##name = enif_make_atom(env,string)


// general
DECL_ATOM(ok);
DECL_ATOM(true);
DECL_ATOM(false);
DECL_ATOM(undefined);
DECL_ATOM(error);

// wave header fields
DECL_ATOM(format);
DECL_ATOM(rate);
DECL_ATOM(size);
DECL_ATOM(channels);
DECL_ATOM(channel_map);

// channel map
DECL_ATOM(silent);          // SND_CHMAP_NA
DECL_ATOM(mono);            // SND_CHMAP_MOMO
DECL_ATOM(left);            // SND_CHMAP_FL (alias)
DECL_ATOM(right);           // SND_CHMAP_FR (alias)
DECL_ATOM(front_left);      // SND_CHMAP_FL
DECL_ATOM(front_right);     // SND_CHMAP_FR
DECL_ATOM(rear_left);       // SND_CHMAP_RL
DECL_ATOM(rear_right);      // SND_CHMAP_RR
DECL_ATOM(front_center);    // SND_CHMAP_FC
DECL_ATOM(lfe);             // SND_CHMAP_LFE
DECL_ATOM(side_left);       // SND_CHMAP_SL
DECL_ATOM(side_right);      // SND_CHMAP_SR

//
// wave sample format
DECL_ATOM(s8);
DECL_ATOM(u8);
DECL_ATOM(s16_le);
DECL_ATOM(s16_be);
DECL_ATOM(u16_le);
DECL_ATOM(u16_be);
DECL_ATOM(s24_le);
DECL_ATOM(s24_be);
DECL_ATOM(u24_le);
DECL_ATOM(u24_be);
DECL_ATOM(s32_le);
DECL_ATOM(s32_be);
DECL_ATOM(u32_le);
DECL_ATOM(u32_be);
DECL_ATOM(float_le);
DECL_ATOM(float_be);
DECL_ATOM(float64_le);
DECL_ATOM(float64_be);
DECL_ATOM(iec958_subframe_le);
DECL_ATOM(iec958_subframe_be);
DECL_ATOM(mu_law);
DECL_ATOM(a_law);
DECL_ATOM(ima_adpcm);
DECL_ATOM(mpeg);
DECL_ATOM(gsm);
DECL_ATOM(s20_le);
DECL_ATOM(s20_be);
DECL_ATOM(u20_le);
DECL_ATOM(u20_be);
DECL_ATOM(special);
DECL_ATOM(s24_3le);
DECL_ATOM(s24_3be);
DECL_ATOM(u24_3le);
DECL_ATOM(u24_3be);
DECL_ATOM(s20_3le);
DECL_ATOM(s20_3be);
DECL_ATOM(u20_3le);
DECL_ATOM(u20_3be);
DECL_ATOM(s18_3le);
DECL_ATOM(s18_3be);
DECL_ATOM(u18_3le);
DECL_ATOM(u18_3be);

// Dirty optional since 2.7 and mandatory since 2.12
#if (ERL_NIF_MAJOR_VERSION > 2) || ((ERL_NIF_MAJOR_VERSION == 2) && (ERL_NIF_MINOR_VERSION >= 7))
#ifdef USE_DIRTY_SCHEDULER
#define NIF_FUNC(name,arity,fptr) {(name),(arity),(fptr),(ERL_NIF_DIRTY_JOB_CPU_BOUND)}
#define NIF_DIRTY_FUNC(name,arity,fptr) {(name),(arity),(fptr),(ERL_NIF_DIRTY_JOB_CPU_BOUND)}
#else
#define NIF_FUNC(name,arity,fptr) {(name),(arity),(fptr),(0)}
#define NIF_DIRTY_FUNC(name,arity,fptr) {(name),(arity),(fptr),(ERL_NIF_DIRTY_JOB_CPU_BOUND)}
#endif
#else
#define NIF_FUNC(name,arity,fptr) {(name),(arity),(fptr)}
#define NIF_DIRTY_FUNC(name,arity,fptr) {(name),(arity),(fptr)}
#endif

static int nif_load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info);
static int nif_upgrade(ErlNifEnv* env, void** priv_data, void** old_priv_data,
			 ERL_NIF_TERM load_info);
static void nif_unload(ErlNifEnv* env, void* priv_data);

#define NIF_LIST \
    NIF("list_voices", 0, nif_list_voices) \
    NIF("text_to_wave",3, nif_text_to_wave)

// Declare all nif functions
#undef NIF
#ifdef NIF_TRACE
#define NIF(name, arity, func)						\
    static ERL_NIF_TERM func(ErlNifEnv* env, int argc,const ERL_NIF_TERM argv[]); \
    static ERL_NIF_TERM trace##_##func##_##arity(ErlNifEnv* env, int argc,const ERL_NIF_TERM argv[]);
#else
#define NIF(name, arity, func)						\
    static ERL_NIF_TERM func(ErlNifEnv* env, int argc,const ERL_NIF_TERM argv[]);
#endif

NIF_LIST

// may be needed to initailize gpu!?
#ifdef __APPLE__
extern int erl_drv_stolen_main_thread_join(ErlNifTid tid, void **respp);
extern int erl_drv_steal_main_thread(char *name,
				     ErlNifTid *dtid,
				     void* (*func)(void*),
				     void* arg,
				     ErlDrvThreadOpts *opts);
#endif


#undef NIF
#ifdef NIF_TRACE
#define NIF(name,arity,func) NIF_FUNC(name, arity, trace##_##func##_##arity),
#else
#define NIF(name,arity,func) NIF_FUNC(name, arity, func),
#endif

ErlNifFunc nif_funcs[] =
{
    NIF_LIST
};

typedef struct _voice_t
{
    struct _voice_t* next;
    char* lang;
    char* voice;
    cst_voice *v;
} voice_t;

typedef struct {
    int ref_count;
    int debug;             // current debug level
    voice_t* first_voice;
} flite_ctx_t;

static void load_atoms(ErlNifEnv* env,flite_ctx_t* ctx);

int debug_level = DLOG_DEFAULT;

void emit_log(int level, char* file, int line, ...)
{
    va_list ap;
    char* fmt;

    if ((level == DLOG_EMERGENCY) ||
	((debug_level >= 0) && (level <= debug_level))) {
	int save_errno = errno;
	    
	va_start(ap, line);
	fmt = va_arg(ap, char*);
	fprintf(stderr, "%s:%d: ", file, line); 
	vfprintf(stderr, fmt, ap);
	fprintf(stderr, "\r\n");
	va_end(ap);
	errno = save_errno;
    }
}

static void set_debug(int level)
{
    debug_level = level;
}


static ERL_NIF_TERM make_wave_header(ErlNifEnv* env, cst_wave* wav)
{
    return 
	enif_make_list3(
	    env,
	    enif_make_tuple2(env, ATOM(format),
			     ATOM(s16_le)),
	    enif_make_tuple2(env, ATOM(rate),
			     enif_make_int(env, wav->sample_rate)),
	    enif_make_tuple2(env, ATOM(channels),
			     enif_make_int(env,wav->num_channels)));
}


// return list of [{Lang::string(),Voice::string()}]
static ERL_NIF_TERM nif_list_voices(ErlNifEnv* env, int argc,
				    const ERL_NIF_TERM argv[])
{
    (void) argc;
    flite_ctx_t* ctx = (flite_ctx_t*) enif_priv_data(env);
    voice_t* vp = ctx->first_voice;
    ERL_NIF_TERM list = enif_make_list(env, 0);

    while(vp) {
	ERL_NIF_TERM v, l;
	ERL_NIF_TERM elem;

	l = enif_make_string(env,vp->lang,ERL_NIF_LATIN1);
	v = enif_make_string(env,vp->voice,ERL_NIF_LATIN1);
	elem = enif_make_tuple2(env, l, v);
	list = enif_make_list_cell(env, elem, list);
	vp = vp->next;	    
    }
    return list;
}

static ERL_NIF_TERM nif_text_to_wave(ErlNifEnv* env, int argc,
				     const ERL_NIF_TERM argv[])
{
    (void) argc;
    flite_ctx_t* ctx = (flite_ctx_t*) enif_priv_data(env);
    voice_t* vp = ctx->first_voice;
    ErlNifBinary text;
    ErlNifBinary samples;
    cst_voice *v;
    cst_wave* wav;
    char lang[128];
    char voice[128];
    int r;
    size_t size;
    ERL_NIF_TERM data;
    ERL_NIF_TERM header;    
    ERL_NIF_TERM zarg = enif_make_list(env, 0);

    // add a zero at end of iolist!
    zarg = enif_make_list_cell(env, enif_make_int(env, 0), zarg);
    zarg = enif_make_list_cell(env, argv[0], zarg);
    if (!enif_inspect_iolist_as_binary(env, zarg, &text))
	return enif_make_badarg(env);
    if ((r = enif_get_string(env, argv[1], lang, sizeof(lang),
			     ERL_NIF_LATIN1)) <= 0)
	return enif_make_badarg(env);
    if ((r = enif_get_string(env, argv[2], voice, sizeof(voice),
			     ERL_NIF_LATIN1)) <= 0)
	return enif_make_badarg(env);

    while(vp) {
	if (((lang[0] == '\0') || (strcmp(lang, vp->lang) == 0)) ||
	    ((voice[0] == '\0') || (strcmp(voice, vp->voice) == 0))) {
	    v = vp->v;
	    break;
	}
	vp = vp->next;
    }
    if (vp == NULL) // not found
	return ATOM(false);

    wav = flite_text_to_wave((const char*) text.data, v);
    size = wav->num_channels*wav->num_samples*2; // always 16bit samples?
    if (!enif_alloc_binary(size, &samples))
	return enif_make_badarg(env);
    memcpy(samples.data, wav->samples, size);
    data = enif_make_binary(env, &samples);
    enif_release_binary(&samples);

    header = make_wave_header(env, wav);
    delete_wave(wav);
    return enif_make_tuple2(env, header, data);
}
	    
// create all tracing NIFs
#ifdef NIF_TRACE

#undef NIF

static void trace_print_arg_list(ErlNifEnv* env,int argc,const ERL_NIF_TERM argv[])
{
    enif_fprintf(stdout, "(");
    if (argc > 0) {
	int i;
	if (enif_is_ref(env, argv[0])) {
	    // FIXME print object type if available
	    enif_fprintf(stdout, "%T", argv[0]);
	}
	else
	    enif_fprintf(stdout, "%T", argv[0]);
	for (i = 1; i < argc; i++)
	    enif_fprintf(stdout, ",%T", argv[i]);
    }
    enif_fprintf(stdout, ")");
}

#define NIF(name, arity, func)					\
static ERL_NIF_TERM trace##_##func##_##arity(ErlNifEnv* env, int argc,const ERL_NIF_TERM argv[]) \
{ \
    ERL_NIF_TERM result;					\
    enif_fprintf(stdout, "ENTER %s", (name));			\
    trace_print_arg_list(env, argc, argv);			\
    enif_fprintf(stdout, "\r\n");				\
    result = func(env, argc, argv);				\
    enif_fprintf(stdout, "  RESULT=%T\r\n", (result));		\
    enif_fprintf(stdout, "LEAVE %s\r\n", (name));		\
    return result;						\
}

NIF_LIST

#endif

static void load_atoms(ErlNifEnv* env,flite_ctx_t* ctx)
{
    (void) ctx;
    // Load atoms
    LOAD_ATOM(ok);
    LOAD_ATOM(true);
    LOAD_ATOM(false);
    LOAD_ATOM(undefined);
    LOAD_ATOM(error);

    LOAD_ATOM(format);
    LOAD_ATOM(rate);
    LOAD_ATOM(size);
    LOAD_ATOM(channels);
    LOAD_ATOM(channel_map);

    LOAD_ATOM(silent);
    LOAD_ATOM(mono);
    LOAD_ATOM(left);
    LOAD_ATOM(right);    
    LOAD_ATOM(front_left);
    LOAD_ATOM(front_right);
    LOAD_ATOM(rear_left);
    LOAD_ATOM(rear_right);
    LOAD_ATOM(front_center);
    LOAD_ATOM(lfe);
    LOAD_ATOM(side_left);
    LOAD_ATOM(side_right);

    LOAD_ATOM(s8);
    LOAD_ATOM(u8);
    LOAD_ATOM(s16_le);
    LOAD_ATOM(s16_be);
    LOAD_ATOM(u16_le);
    LOAD_ATOM(u16_be);
    LOAD_ATOM(s24_le);
    LOAD_ATOM(s24_be);
    LOAD_ATOM(u24_le);
    LOAD_ATOM(u24_be);
    LOAD_ATOM(s32_le);
    LOAD_ATOM(s32_be);
    LOAD_ATOM(u32_le);
    LOAD_ATOM(u32_be);
    LOAD_ATOM(float_le);
    LOAD_ATOM(float_be);
    LOAD_ATOM(float64_le);
    LOAD_ATOM(float64_be);
    LOAD_ATOM(iec958_subframe_le);
    LOAD_ATOM(iec958_subframe_be);
    LOAD_ATOM(mu_law);
    LOAD_ATOM(a_law);
    LOAD_ATOM(ima_adpcm);
    LOAD_ATOM(mpeg);
    LOAD_ATOM(gsm);
    LOAD_ATOM(s20_le);
    LOAD_ATOM(s20_be);
    LOAD_ATOM(u20_le);
    LOAD_ATOM(u20_be);
    LOAD_ATOM(special);
    LOAD_ATOM(s24_3le);
    LOAD_ATOM(s24_3be);
    LOAD_ATOM(u24_3le);
    LOAD_ATOM(u24_3be);
    LOAD_ATOM(s20_3le);
    LOAD_ATOM(s20_3be);
    LOAD_ATOM(u20_3le);
    LOAD_ATOM(u20_3be);
    LOAD_ATOM(s18_3le);
    LOAD_ATOM(s18_3be);
    LOAD_ATOM(u18_3le);
    LOAD_ATOM(u18_3be);    
}

static void dl_error(void* arg,const char* what)
{
    ERRORF("flite dl error: %s", what);
}

static cst_voice *load_voice(const char* lib, char** lang_ptr, char** voice_ptr)
{
    cst_voice *v;
    void* handle;
    char symname[256];
    const char* filename;
    char* ptr;
    char buf[128];
    size_t len;
    char *lang;
    char *voice;
    cst_voice * (*regfn)();
    
    DEBUGF("lib = [%s]", lib);
    
    if ((filename = strrchr(lib, '/')) != NULL)
	filename++;
    else
	filename = lib;
    ptr = (char*)filename;
    if (memcmp(ptr, "libflite_cmu_", 13) != 0)
	return NULL;
    ptr += 13;
    if ((len = strlen(ptr)) >= sizeof(buf))
	return NULL;
    memcpy(buf, ptr, len+1);
    if ((ptr = strrchr(buf, '_')) == NULL) return NULL;
    lang  = buf;
    voice = ptr+1;
    *ptr = '\0';

    snprintf(symname, sizeof(symname), "register_cmu_%s_%s", lang, voice);

    if ((handle = enif_dlopen(lib, dl_error, NULL)) == NULL) {
	ERRORF("unable to dlopen [%s]", lib);
	return NULL;
    }
    if ((regfn = enif_dlsym(handle, symname, dl_error, NULL)) == NULL) {
	ERRORF("unable to find symbol [%s]", symname);
	return NULL;
    }
    if ((v = (*regfn)()) == NULL) {
	ERRORF("unable to register lib [%s]", lib);
	return NULL;
    }
    *lang_ptr = strdup(lang);
    *voice_ptr = strdup(voice);
    return v;
}
    

static int nif_load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    flite_ctx_t* ctx;
    (void) env;
    ERL_NIF_TERM list;
    
    set_debug(DLOG_DEFAULT);

    if ((ctx = (flite_ctx_t*) enif_alloc(sizeof(flite_ctx_t))) == NULL)
	return -1;
    ctx->ref_count = 1;
    ctx->debug = DLOG_DEFAULT;
    ctx->first_voice = NULL;
    
    load_atoms(env, ctx);
    // scan list of libraries to load
    list = load_info;
    while(enif_is_list(env, list) && !enif_is_empty_list(env, list)) {
	ERL_NIF_TERM head;
	ERL_NIF_TERM tail;
	char libpath[MAX_PATH];
	char* lang;
	char* voice;
	cst_voice *v;
	char* ptr;
	int r;

	enif_get_list_cell(env, list, &head, &tail);
	if (!(r = enif_get_string(env, head, libpath, sizeof(libpath),
				  ERL_NIF_LATIN1)) || (r < 0))
	    return -1;
	// remove .so
	if ((ptr = strrchr(libpath, '.')) != NULL) {
	    if (strcmp(ptr, ".so") == 0) 
		*ptr = '\0';
	}
	if ((v = load_voice(libpath, &lang, &voice)) != NULL) {
	    voice_t* vp = malloc(sizeof(voice_t));
	    vp->next = ctx->first_voice;
	    vp->lang = lang;    // dynamic
	    vp->voice = voice;  // dynamic
	    vp->v = v;
	    ctx->first_voice = vp;
	}
	list = tail;
    }
    *priv_data = ctx;
    return 0;
}

static int nif_upgrade(ErlNifEnv* env, void** priv_data, void** old_priv_data,
		       ERL_NIF_TERM load_info)
{
    (void) env;
    (void) load_info;
    flite_ctx_t* ctx = (flite_ctx_t*) *old_priv_data;
    
    ctx->ref_count++;
    load_atoms(env, ctx);
    *priv_data = *old_priv_data;
    return 0;
}

static void nif_unload(ErlNifEnv* env, void* priv_data)
{
    (void) env;
    flite_ctx_t* ctx = (flite_ctx_t*) priv_data;
    voice_t* vp = ctx->first_voice;

    while(vp) {
	voice_t* vpn;
	free(vp->lang);
	free(vp->voice);
	delete_voice(vp->v);
	vpn = vp->next;
	free(vp);
	vp = vpn;
    }
    DEBUGF("nif_unload");
    enif_free(ctx);
}


ERL_NIF_INIT(flite, nif_funcs,
	     nif_load, NULL,
	     nif_upgrade, nif_unload)
