package com.pibbletv.user_service.business.interfaces;

import com.pibbletv.user_service.domain.User;
import reactor.core.publisher.Mono;

public interface UserService {
    Mono<Void> saveUser(String userId, String username);
    Mono<User> getUser(String userId);
}