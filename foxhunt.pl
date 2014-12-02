#!/usr/bin/perl

# The MIT License (MIT)
#
# Copyright (c) 2014 Tim Wilkens
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

use strict;
use warnings;

# Set ENV VAR PERL_LWP_SSL_VERIFY_HOSTNAME to 0
# or get Mozilla::CA

use Getopt::Long;

my $banner = <<EOF;

  ______        _    _             _   
 |  ____|      | |  | |           | |  
 | |__ _____  _| |__| |_   _ _ __ | |_ 
 |  __/ _ \\ \\/ /  __  | | | | '_ \\| __|
 | | | (_) >  <| |  | | |_| | | | | |_ 
 |_|  \\___/_/\\_\\_|  |_|\\__,_|_| |_|\\__|
                                       

EOF

my %work;
my %seen;


my ($seed, $confine);
GetOptions(
  "seed=s"    => \$seed,
  "confine=s" => \$confine,
);

if (!$seed) {
  die "Must provide --seed url.\n";
}

print $banner;

if ($seed !~ /^https?:\/\//) {
  $seed = ("http://" . $seed);
}

$work{$seed} = 1;
my $hound = Hound->new(confine => $confine);

while (my $url = get_work()) {
  print "$url";
  if ($hound->url_is_reflected($url)) {
    if ($hound->payload_is_reflected($url)) {
      print " \033[31mVULNERABLE\033[0m";
    }
  }
  print "\n";
  add_work($hound->get_all_links);
  sleep(3);
}

print "All done. Exiting.\n";

sub add_work {
  my @links = @_;
  for my $l (@links) {
    my $normalized = $l;
    $normalized .= "/" unless ($normalized =~ /\/$/);
    if (!$seen{$normalized}) {
      $work{$l} = 1;
    }
  }
}

sub get_work {
  if (keys %work) {
    my $url = (keys %work)[0];
    delete $work{$url};
    $seen{$url} = 1;
    return $url;
  } else {
    return undef;
  }
}


BEGIN {

package Hound;

use strict;
use warnings;

use LWP::UserAgent;
use HTML::LinkExtor;
use URI::URL;
use HTTP::Request;

my @LINKS;

sub link_parser {
  my($tag, %attr) = @_;
  return if $tag eq 'img';
  push(@LINKS, values %attr);
}

sub new {
  my ($class, %args) = @_;
  my $self = bless {}, $class;
  $self->{fetcher} = LWP::UserAgent->new();
  $self->{fetcher}->max_redirect(0);
  $self->{fetcher}->agent("fake");
  $self->{extractor} = HTML::LinkExtor->new(\&link_parser);
  $self->{payload} = "\"<<>";
  $self->{previous_response} = undef;
  $self->{confine} = $args{confine};
  return $self;
}

sub get_all_links {
  my $self = shift;
  return unless $self->{previous_response};
  my $html = $self->{previous_response}->content;
  $self->{extractor}->parse($html);

  my $base = $self->{previous_response}->base->as_string;
  my @links = grep { $_ !~ /javascript/ }
              map  { $_ = url($_, $base)->abs; } @LINKS;

  if ($self->{confine}) {
    @links = grep { $_ =~ /$self->{confine}/ } @links;
  }

  @LINKS = undef;
  $self->{previous_response} = undef;
  return @links;
}

sub fetch {
  my ($self, $url) = @_;
  $self->update_useragent();
  my $request = HTTP::Request->new('GET' => $url);
  $request->header('Referer' => '');

  return $self->{fetcher}->request($request);
}

sub create_injected {
  my ($self, $url) = @_;
  return ($url =~ /\/$/) ? "$url" . "$self->{payload}/" : "$url/" . "$self->{payload}/";
}

sub url_is_reflected {
  my ($self, $url) = @_;
  my $response = $self->fetch($url);
  $hound->{previous_response} = $response;
  return 0 if (!$response->is_success);
  my $content = $response->content;
  my $u = $response->base->as_string;
  return ($content =~ /$u/) ? 1 : 0;
}

sub payload_is_reflected {
  my ($self, $url) = @_;
  my $inject_url = $self->create_injected($url);
  my $response = $hound->fetch($inject_url);
  return 0 if (!$response->is_success);
  my $content = $response->content;
  return ($content =~ /(?<!\\)$self->{payload}/) ? 1 : 0;
}

sub update_useragent {
  my $self = shift;

  my @useragents = (
    "Mozilla/4.0 (compatible; MSIE 7.0; Windows NT 5.1; SV1; .NET CLR 1.1.4322) Maxthon",
    "Mozilla/4.0 (compatible; MSIE 7.0; Windows NT 6.1; Trident/5.0; SLCC2; .NET CLR 2.0.50727; .NET CLR 3.5.30729; .NET CLR 3.0.30729; Media Center PC 6.0; InfoPath.3)",
    "Mozilla/5.0 (Android; Mobile; rv:34.0) Gecko/34.0 Firefox/34.0",
    "Mozilla/5.0 (compatible; MSIE 10.0; Windows NT 6.1; Trident/6.0)",
    "Mozilla/5.0 (compatible; MSIE 10.0; Windows NT 6.1; WOW64; Trident/6.0)",
    "Mozilla/5.0 (compatible; MSIE 10.0; Windows NT 6.2; Trident/6.0; MSAppHost/1.0)",
    "Mozilla/5.0 (compatible; MSIE 10.0; Windows NT 6.2; WOW64; Trident/6.0; Touch)",
    "Mozilla/5.0 (compatible; MSIE 9.0; Windows NT 6.0; Trident/5.0; BOIE9;ENUS)",
    "Mozilla/5.0 (compatible; MSIE 9.0; Windows NT 6.1; Trident/5.0)",
    "Mozilla/5.0 (compatible; MSIE 9.0; Windows NT 6.1; WOW64; Trident/5.0)",
    "Mozilla/5.0 (compatible; MSIE 9.0; Windows NT 6.1; WOW64; Trident/5.0; yie9)",
    "Mozilla/5.0 (compatible; MSIE 9.0; Windows NT 6.3; WOW64; Trident/7.0; Touch)",
    "Mozilla/5.0 (iPad; CPU OS 5_1_1 like Mac OS X) AppleWebKit/534.46 (KHTML, like Gecko) Version/5.1 Mobile/9B206 Safari/7534.48.3",
    "Mozilla/5.0 (iPad; CPU OS 7_0_4 like Mac OS X) AppleWebKit/537.51.1 (KHTML, like Gecko) Mobile/11B554a",
    "Mozilla/5.0 (iPad; CPU OS 7_1_1 like Mac OS X) AppleWebKit/537.51.2 (KHTML, like Gecko) Version/7.0 Mobile/11D201 Safari/9537.53",
    "Mozilla/5.0 (iPad; CPU OS 7_1_2 like Mac OS X) AppleWebKit/537.51.2 (KHTML, like Gecko) Version/7.0 Mobile/11D257 Safari/9537.53",
    "Mozilla/5.0 (iPad; CPU OS 7_1 like Mac OS X) AppleWebKit/537.51.2 (KHTML, like Gecko) Version/7.0 Mobile/11D167 Safari/9537.53",
    "Mozilla/5.0 (iPad; CPU OS 8_0_2 like Mac OS X) AppleWebKit/600.1.4 (KHTML, like Gecko) Version/8.0 Mobile/12A405 Safari/600.1.4",
    "Mozilla/5.0 (iPad; CPU OS 8_1_1 like Mac OS X) AppleWebKit/600.1.4 (KHTML, like Gecko) Mobile/12B435",
    "Mozilla/5.0 (iPad; CPU OS 8_1_1 like Mac OS X) AppleWebKit/600.1.4 (KHTML, like Gecko) Version/8.0 Mobile/12B435 Safari/600.1.4",
    "Mozilla/5.0 (iPad; CPU OS 8_1 like Mac OS X) AppleWebKit/600.1.4 (KHTML, like Gecko) Version/8.0 Mobile/12B410 Safari/600.1.4",
    "Mozilla/5.0 (iPhone; CPU iPhone OS 5_0 like Mac OS X) AppleWebKit/534.46 (KHTML, like Gecko) Version/5.1 Mobile/9A334 Safari/7534.48.3",
    "Mozilla/5.0 (iPhone; CPU iPhone OS 7_0_4 like Mac OS X) AppleWebKit/537.51.1 (KHTML, like Gecko) Version/7.0 Mobile/11B554a Safari/9537.53",
    "Mozilla/5.0 (iPhone; CPU iPhone OS 7_0 like Mac OS X) AppleWebKit/537.51.1 (KHTML, like Gecko) Version/7.0 Mobile/11A465 Safari/9537.53 BingPreview/1.0b",
    "Mozilla/5.0 (iPhone; CPU iPhone OS 7_1_1 like Mac OS X) AppleWebKit/537.51.2 (KHTML, like Gecko) Version/7.0 Mobile/11D201 Safari/9537.53",
    "Mozilla/5.0 (iPhone; CPU iPhone OS 7_1_2 like Mac OS X) AppleWebKit/537.51.2 (KHTML, like Gecko) Mobile/11D257 [FBAN/FBIOS;FBAV/18.1.0.14.11;FBBV/5295262;FBDV/iPhone3,3;FBMD/iPhone;FBSN/iPhone OS;FBSV/7.1.2;FBSS/2; FBCR/TFW;FBID/phone;FBLC/en_US;FBOP/5]",
    "Mozilla/5.0 (iPhone; CPU iPhone OS 7_1_2 like Mac OS X) AppleWebKit/537.51.2 (KHTML, like Gecko) Version/7.0 Mobile/11D257 Safari/9537.53",
    "Mozilla/5.0 (iPhone; CPU iPhone OS 7_1 like Mac OS X) AppleWebKit/537.51.2 (KHTML, like Gecko) Version/7.0 Mobile/11D167 Safari/9537.53",
    "Mozilla/5.0 (iPhone; CPU iPhone OS 8_0_2 like Mac OS X) AppleWebKit/600.1.4 (KHTML, like Gecko) Mobile/12A405 [FBAN/FBIOS;FBAV/18.1.0.14.11;FBBV/5295262;FBDV/iPhone5,3;FBMD/iPhone;FBSN/iPhone OS;FBSV/8.0.2;FBSS/2; FBCR/Verizon;FBID/phone;FBLC/en_US;FBOP/5]",
    "Mozilla/5.0 (iPhone; CPU iPhone OS 8_0_2 like Mac OS X) AppleWebKit/600.1.4 (KHTML, like Gecko) Version/8.0 Mobile/12A405 Safari/600.1.4",
    "Mozilla/5.0 (iPhone; CPU iPhone OS 8_0 like Mac OS X) AppleWebKit/600.1.4 (KHTML, like Gecko) Version/8.0 Mobile/12A365 Safari/600.1.4",
    "Mozilla/5.0 (iPhone; CPU iPhone OS 8_1_1 like Mac OS X) AppleWebKit/600.1.4 (KHTML, like Gecko) Mobile/12B435",
    "Mozilla/5.0 (iPhone; CPU iPhone OS 8_1_1 like Mac OS X) AppleWebKit/600.1.4 (KHTML, like Gecko) Version/8.0 Mobile/12B435 Safari/600.1.4",
    "Mozilla/5.0 (iPhone; CPU iPhone OS 8_1_1 like Mac OS X) AppleWebKit/600.1.4 (KHTML, like Gecko) Version/8.0 Mobile/12B436 Safari/600.1.4",
    "Mozilla/5.0 (iPhone; CPU iPhone OS 8_1 like Mac OS X) AppleWebKit/600.1.4 (KHTML, like Gecko) Mobile/12B411",
    "Mozilla/5.0 (iPhone; CPU iPhone OS 8_1 like Mac OS X) AppleWebKit/600.1.4 (KHTML, like Gecko) Version/8.0 Mobile/12B411 Safari/600.1.4",
    "Mozilla/5.0 (Linux; Android 4.2.2; Galaxy Nexus Build/JDQ39E) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/39.0.2171.59 Mobile Safari/537.36",
    "Mozilla/5.0 (Linux; Android 4.3; C6606 Build/10.4.C.0.814) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/39.0.2171.59 Mobile Safari/537.36",
    "Mozilla/5.0 (Linux; Android 4.4.2; en-us; SAMSUNG-SGH-I527 Build/KOT49H) AppleWebKit/537.36 (KHTML, like Gecko) Version/1.5 Chrome/28.0.1500.94 Mobile Safari/537.36",
    "Mozilla/5.0 (Linux; Android 4.4.2; en-us; SAMSUNG-SM-G900A Build/KOT49H) AppleWebKit/537.36 (KHTML, like Gecko) Version/1.6 Chrome/28.0.1500.94 Mobile Safari/537.36",
    "Mozilla/5.0 (Linux; Android 4.4.2; en-us; SAMSUNG SM-G900T Build/KOT49H) AppleWebKit/537.36 (KHTML, like Gecko) Version/1.6 Chrome/28.0.1500.94 Mobile Safari/537.36",
    "Mozilla/5.0 (Linux; Android 4.4.2; LG-D851 Build/KVT49L.D85110m) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/39.0.2171.59 Mobile Safari/537.36",
    "Mozilla/5.0 (Linux; Android 4.4.2; SAMSUNG-SGH-I337 Build/KOT49H) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/39.0.2171.59 Mobile Safari/537.36",
    "Mozilla/5.0 (Linux; Android 4.4.2; SGH-T399N Build/KOT49H) AppleWebKit/537.36 (KHTML, like Gecko) Version/4.0 Chrome/30.0.0.0 Mobile Safari/537.36",
    "Mozilla/5.0 (Linux; Android 4.4.2; SM-G730W8 Build/KOT49H) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/33.0.1750.517 Mobile Safari/537.36",
    "Mozilla/5.0 (Linux; Android 4.4.2; SM-G900T Build/KOT49H) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/39.0.2171.59 Mobile Safari/537.36",
    "Mozilla/5.0 (Linux; Android 4.4.4; en-us; SAMSUNG-SGH-I337 Build/KTU84P) AppleWebKit/537.36 (KHTML, like Gecko) Version/1.5 Chrome/28.0.1500.94 Mobile Safari/537.36",
    "Mozilla/5.0 (Linux; Android 4.4.4; XT1080 Build/SU5-24) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/39.0.2171.59 Mobile Safari/537.36",
    "Mozilla/5.0 (Linux; U; Android 4.0.3; en-us; KFTT Build/IML74K) AppleWebKit/537.36 (KHTML, like Gecko) Silk/3.35 like Chrome/34.0.1847.137 Safari/537.36",
    "Mozilla/5.0 (Linux; U; Android 4.1.2; en-us; SCH-I915 Build/JZO54K) AppleWebKit/534.30 (KHTML, like Gecko) Version/4.0 Safari/534.30",
    "Mozilla/5.0 (Linux; U; Android 4.2.2; en-us; ME302C Build/JDQ39) AppleWebKit/534.30 (KHTML, like Gecko) Version/4.0 Safari/534.30",
    "Mozilla/5.0 (Linux; U; Android 4.3; en-us; Z730 Build/JLS36C) AppleWebKit/534.30 (KHTML, like Gecko) Version/4.0 Mobile Safari/534.30",
    "Mozilla/5.0 (Linux; U; Android 4.4.4; en-us; SM-G900V Build/KTU84P) AppleWebKit/534.30 (KHTML, like Gecko) Version/4.0 Mobile Safari/534.30",
    "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_0) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/39.0.2171.71 Safari/537.36",
    "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_1) AppleWebKit/600.1.25 (KHTML, like Gecko) Version/8.0 Safari/600.1.25",
    "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10) AppleWebKit/600.1.25 (KHTML, like Gecko) Version/8.0 Safari/600.1.25",
    "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_5_8) AppleWebKit/534.50.2 (KHTML, like Gecko) Version/5.0.6 Safari/533.22.3",
    "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.6; rv:33.0) Gecko/20100101 Firefox/33.0",
    "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_7_5) AppleWebKit/536.30.1 (KHTML, like Gecko) Version/6.0.5 Safari/536.30.1",
    "Mozilla/5.0 (Macintosh); Intel Mac OS X 10_7_5) AppleWebKit/537.11 (KHTML, like Gecko) Chrome/23.0.1271.97 Safari/537.11",
    "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.8; rv:33.0) Gecko/20100101 Firefox/33.0",
    "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_9_4) AppleWebKit/537.77.4 (KHTML, like Gecko) Version/7.0.5 Safari/537.77.4",
    "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_9_5) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/39.0.2171.65 Safari/537.36",
    "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_9_5) AppleWebKit/600.1.17 (KHTML, like Gecko) Version/7.1 Safari/537.85.10",
    "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.9; rv:28.0) Gecko/20100101 Firefox/28.0 (FlipboardProxy/1.1; +http://flipboard.com/browserproxy)",
    "Mozilla/5.0 (Macintosh; U; Intel Mac OS X 10_6_3; en-us; Silk/1.0.143.1-Gen4_11004910) AppleWebKit/533.16 (KHTML, like Gecko) Version/5.0 Safari/533.16 Silk-Accelerated=true",
    "Mozilla/5.0 (Windows NT 5.1) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/39.0.2171.65 Safari/537.36",
    "Mozilla/5.0 (Windows NT 5.1) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/39.0.2171.71 Safari/537.36",
    "Mozilla/5.0 (Windows NT 5.1; rv:11.0) Gecko Firefox/11.0 (via ggpht.com GoogleImageProxy)",
    "Mozilla/5.0 (Windows NT 5.1; rv:33.0) Gecko/20100101 Firefox/33.0",
    "Mozilla/5.0 (Windows NT 6.1) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/39.0.2171.71 Safari/537.36",
    "Mozilla/5.0 (Windows NT 6.1; Trident/7.0; rv:11.0) like Gecko",
    "Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/39.0.2171.65 Safari/537.36",
    "Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/39.0.2171.71 Safari/537.36",
    "Mozilla/5.0 (Windows NT 6.1; WOW64; rv:10.0) Gecko/20100101 Firefox/10.0",
    "Mozilla/5.0 (Windows NT 6.1; WOW64; rv:33.0) Gecko/20100101 Firefox/33.0",
    "Mozilla/5.0 (Windows NT 6.1; WOW64; Trident/7.0; MDDRJS; rv:11.0) like Gecko",
    "Mozilla/5.0 (Windows NT 6.1; WOW64; Trident/7.0; rv:11.0) like Gecko",
    "Mozilla/5.0 (Windows NT 6.2; Win64; x64) Gecko/20120403211507 Firefox/12.0",
    "Mozilla/5.0 (Windows NT 6.2; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/39.0.2171.71 Safari/537.36",
    "Mozilla/5.0 (Windows NT 6.2; WOW64; rv:33.0) Gecko/20100101 Firefox/33.0",
    "Mozilla/5.0 (Windows NT 6.3; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/38.0.2125.111 Safari/537.36",
    "Mozilla/5.0 (Windows NT 6.3; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/39.0.2171.65 Safari/537.36",
    "Mozilla/5.0 (Windows NT 6.3; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/39.0.2171.71 Safari/537.36",
    "Mozilla/5.0 (Windows NT 6.3; WOW64; rv:31.0) Gecko/20100101 Firefox/31.0",
    "Mozilla/5.0 (Windows NT 6.3; WOW64; rv:33.0) Gecko/20100101 Firefox/33.0",
    "Mozilla/5.0 (Windows NT 6.3; WOW64; Trident/7.0; rv:11.0) like Gecko",
    "Mozilla/5.0 (Windows NT 6.3; WOW64; Trident/7.0; Touch; rv:11.0) like Gecko",
    "Mozilla/5.0 (Windows Phone 8.1; ARM; Trident/7.0; Touch; rv:11.0; IEMobile/11.0; NOKIA; Lumia 635) like Gecko",
    "Mozilla/5.0 (X11; Linux i686) AppleWebKit/537.36 (KHTML, like Gecko) Ubuntu Chromium/28.0.1500.52 Chrome/28.0.1500.52 Safari/537.36",
    "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.22 (KHTML, like Gecko) Ubuntu Chromium/25.0.1364.160 Chrome/25.0.1364.160 Safari/537.22",
    "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/36.0.1985.67 Safari/537.36",
    "Opera/9.80 (Windows NT 6.2; Win64; x64) Presto/2.12.388 Version/12.16"
  );

  $self->{fetcher}->agent($useragents[rand(@useragents)]);
}

}
