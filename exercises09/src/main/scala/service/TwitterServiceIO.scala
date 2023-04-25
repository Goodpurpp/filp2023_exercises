package service

import cats.effect.IO
import cats.syntax.all._
import service.TwitterService
import service.domain.GetTweetResponse.{Found, NotFound}
import service.domain._
import twitter.TwitterApi
import twitter.domain.TwitterError._
import twitter.domain._

import scala.util.{Failure, Success, Try}

// Воспользуйтесь синтаксисом map, recover, traverse из cats.syntax.all_
class TwitterServiceIO(api: TwitterApi) extends TwitterService[IO] {
  def tweet(user: User, text: String): IO[TweetId] = {
    io[TweetId](api.tweet(user, text))
  }

  def like(user: User, tweetId: TweetId): IO[Unit] = {
    io[Unit](api.like(user, tweetId)).recover {
      case LikeAlreadyExistError => ()
    }
  }

  def unlike(user: User, tweetId: TweetId): IO[Unit] = {
    io(api.unlike(user, tweetId)).recover {
      case LikeNotExistError => ()
    }
  }

  def getTweet(tweetId: TweetId): IO[GetTweetResponse] = {
    io[TweetInfo](api.get(tweetId)).redeem(_ => NotFound(tweetId), x => Found(x))
  }

  def io[T](f: (Try[T] => Unit) => Unit): IO[T] = {
    IO.async(cb => f(cb.compose(_.toEither)))
  }

  def getTweets(ids: List[TweetId]): IO[GetTweetsResponse] =
    ids
      .traverse(getTweet)
      .map(x =>
        x.foldLeft(GetTweetsResponse(Set.empty[TweetId], Set.empty[TweetInfo]))((tweetsResponse, current) => {
          val notFound = tweetsResponse.notFound
          val found    = tweetsResponse.found
          current match {
            case Found(v)    => GetTweetsResponse(notFound, found + v)
            case NotFound(v) => GetTweetsResponse(notFound + v, found)
          }
        })
      )
}
