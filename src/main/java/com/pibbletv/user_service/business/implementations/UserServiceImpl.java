package com.pibbletv.user_service.business.implementations;

import com.pibbletv.user_service.business.interfaces.UserService;
import com.pibbletv.user_service.persistance.jpa.UserRepository;
import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;

@Service
@AllArgsConstructor
public class UserServiceImpl implements UserService {

    private final UserRepository userRepository;
}
