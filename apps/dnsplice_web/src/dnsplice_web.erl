%% @doc DNSplice_web interface module
%%
%% This module provides the public interface to the DNSplice_web application,
%% and is the primary way that other apps should access its functionality.
%%
%% @end

-module(dnsplice_web).
%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------
-export([
	start/0,
	stop/0
]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

%% @doc
%%
%% starts dnsplice_web
%%
%% @end

-spec start() -> {ok, Started :: list()}.

start() -> application:ensure_all_started(dnsplice_web).

%% @doc
%%
%% stops dnsplice_web
%%
%% @end

-spec stop() -> ok.

stop() -> application:stop(dnsplice_web).
