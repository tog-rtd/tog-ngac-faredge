/********* Sample code generated by the curl command line tool **********
 * All curl_easy_setopt() options are documented at:
 * https://curl.haxx.se/libcurl/c/curl_easy_setopt.html
 ************************************************************************/
#include <curl/curl.h>

int main(int argc, char *argv[])
{
  CURLcode ret;
  CURL *hnd;
  struct curl_slist *slist1;

  slist1 = NULL;
  slist1 = curl_slist_append(slist1, "Authorization: OAuth eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJzdWIiOiJyLmRlbG9uZ0BvcGVuZ3JvdXAub3JnIiwiaWF0IjoxNTU2ODMwMTY3LCJleHAiOjE1NTk0MjIxNjd9.BAshfdS2fdvqN-gkr2FP0PoTVPvML614UtORAYf6A5Y");
  slist1 = curl_slist_append(slist1, "Content-Type: text/plain");

  hnd = curl_easy_init();
  curl_easy_setopt(hnd, CURLOPT_URL, "http://141.58.0.8:2777/new_log?code=111&ip=1.1.1.1&message=Testagain&user=rjd@tog.org");
  curl_easy_setopt(hnd, CURLOPT_NOPROGRESS, 1L);
  curl_easy_setopt(hnd, CURLOPT_USERAGENT, "curl/7.54.0");
  curl_easy_setopt(hnd, CURLOPT_HTTPHEADER, slist1);
  curl_easy_setopt(hnd, CURLOPT_MAXREDIRS, 50L);
  curl_easy_setopt(hnd, CURLOPT_HTTP_VERSION, (long)CURL_HTTP_VERSION_2TLS);
  curl_easy_setopt(hnd, CURLOPT_CUSTOMREQUEST, "POST");
  curl_easy_setopt(hnd, CURLOPT_VERBOSE, 1L);
  curl_easy_setopt(hnd, CURLOPT_TCP_KEEPALIVE, 1L);

  /* Here is a list of options the curl code used that cannot get generated
     as source easily. You may select to either not use them or implement
     them yourself.

  CURLOPT_WRITEDATA set to a objectpointer
  CURLOPT_INTERLEAVEDATA set to a objectpointer
  CURLOPT_WRITEFUNCTION set to a functionpointer
  CURLOPT_READDATA set to a objectpointer
  CURLOPT_READFUNCTION set to a functionpointer
  CURLOPT_SEEKDATA set to a objectpointer
  CURLOPT_SEEKFUNCTION set to a functionpointer
  CURLOPT_ERRORBUFFER set to a objectpointer
  CURLOPT_STDERR set to a objectpointer
  CURLOPT_DEBUGFUNCTION set to a functionpointer
  CURLOPT_DEBUGDATA set to a objectpointer
  CURLOPT_HEADERFUNCTION set to a functionpointer
  CURLOPT_HEADERDATA set to a objectpointer

  */

  ret = curl_easy_perform(hnd);

  curl_easy_cleanup(hnd);
  hnd = NULL;
  curl_slist_free_all(slist1);
  slist1 = NULL;

  return (int)ret;
}
/**** End of sample code ****/
