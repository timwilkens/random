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

sub update_useragent {
  my $self = shift;
  # TODO
#  $self->{nose}->agent_alias($AGENTS[rand(@AGENTS)]);
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

}
